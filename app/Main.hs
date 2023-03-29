{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- Thanks for archive.org to find free books in txt format --

-- TODO LIST --
-- See todo in StatsFromTxt
-- Writing the predictive part of the spell-checker

module Main (main) where

import Control.Monad.Cont
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, decodeStrict, parseJSON, withObject, (.:))
import Data.ByteString.Char8 (pack)
import Data.List (intersperse, sort, sortOn)
import Data.Maybe (mapMaybe)
import Data.Vector (Vector, fromList, imap, (!), (!?))
import qualified Data.Vector as V (find, map)
import System.IO (IOMode (ReadMode), hSetBuffering, readFile)
import WordsTrees

-- import Control.Parallel ( par, pseq )

type Keyboard = Vector Char

instance FromJSON CountedWords where
  parseJSON = withObject "CountedWords" $ \v ->
    CountedWords
      <$> v
      .: "word"
      <*> v
      .: "properties"

main :: IO ()
main = do
  contents <- readFile "app//Statistics//result.txt"
  let inputFreq = words contents
  let dictionaryTree = listToTree $ mapMaybe (decodeStrict . pack) inputFreq
  putStrLn "Type enter to correct a word or tab + enter to complete the current word."
  putStrLn "Type a word:"
  (`runContT` return) $ do
    callCC $ prompt dictionaryTree

prompt :: Tree Char -> (() -> ContT r IO ()) -> ContT r IO ()
prompt tree exit = do
  restart <- label_
  line <- liftIO $ putStr "> " >> getLine
  if null line
    then exit ()
    else do
      liftIO $
        print $
          if last line == '\t'
            then completeWord tree $ init line
            else correctWord tree line
      restart

completeWord :: Tree Char -> String -> [CountedWords]
completeWord tree prefixe = take 10 . sort $ giveSuffixe tree prefixe

correctWord :: Tree Char -> String -> [CountedWords]
correctWord tree word = take 10 . sortOn (\x -> (strDiff (CountedWords word (0, [])) x, x)) $ similarWords tree 2 word

correctLine :: Tree Char -> String -> CountedWords
correctLine tree line = assemble correctWords
  where
    correctWords = map (head . correctWord tree) $ words line
    assemble = foldl (\w1 w2 -> CountedWords (show w1 ++ " " ++ show w2) (-1, [])) (CountedWords "" (-1, []))

prettyPrint :: [String] -> IO ()
prettyPrint l = mapM_ putStr $ ["["] ++ intersperse ", " l ++ ["]\n"]

keyboardEn :: Keyboard
keyboardEn =
  fromList
    [ 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p',
      'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';',
      'z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.'
    ]

-- Define the zone of near characters
nearIndices :: Integral a => a -> [a]
nearIndices ind = case ind `mod` 10 of
  0 -> [ind + 1, ind + 10, ind - 10, ind - 9, ind + 11]
  9 -> [ind - 1, ind + 10, ind - 10, ind + 9, ind - 11]
  _ -> [ind - 1, ind + 1, ind + 10, ind - 10, ind - 11, ind + 11, ind - 9, ind + 9]

-- Gives near chars for each one on the keyboard
charsPerimeter :: Keyboard -> Vector [Char]
charsPerimeter keyboard = imap (\ind _ -> mapMaybe (keyboard !?) $ nearIndices ind) keyboard

-- Association between chars and his neighboors
associateNearChars :: Keyboard -> Vector [Char] -> Vector (Char, [Char])
associateNearChars keyboard perimeter = imap (\ind char -> (char, perimeter ! ind)) keyboard

-- Gives the neighboors of all chars
nearChars :: Keyboard -> Vector (Char, [Char])
nearChars keyboard = associateNearChars keyboard $ charsPerimeter keyboard

-- Used keyboard
actualKeyboard :: Vector (Char, [Char])
actualKeyboard = nearChars keyboardEn

outMaybeAssocList :: Maybe (a, [b]) -> [b]
outMaybeAssocList Nothing = []
outMaybeAssocList (Just (_, l)) = l

-- Calcul of the distance between two words (Hamming's distance modifed)
strDiff :: CountedWords -> CountedWords -> Int
strDiff (CountedWords x _) (CountedWords [] _) = length x
strDiff (CountedWords [] _) (CountedWords y _) = length y
strDiff wx@(CountedWords (x : xs) freqx) wy@(CountedWords (y : ys) freqy) =
  if x == y
    then strDiff (CountedWords xs freqx) (CountedWords ys freqy)
    else 2 + diffMin
  where
    diffMin = min diffMinq $ min (strDiff (CountedWords xs freqx) wy) (strDiff wx (CountedWords ys freqy))
    -- Case where they are "near" :
    diffMinq =
      strDiff (CountedWords xs freqx) (CountedWords ys freqy)
        - if elem x $ nearChar y
          then 1
          else 0
    nearChar c = outMaybeAssocList $ V.find (\z -> c == fst z) actualKeyboard
