{-# LANGUAGE OverloadedStrings, DeriveFunctor #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- --------------------------------------------------------------------------
-- |
-- Module      :  Main
--
--
--  CLI spell checker.
--
-----------------------------------------------------------------------------


-- Thanks to archive.org for indexing free books in txt format --

-- TODO LIST --
-- See todo in StatsFromTxt
-- Writing the predictive part of the spell-checker
-- Better workaround for buffer on windows

module Main (main) where

import Control.Monad.Cont (ContT(runContT), label_, MonadCont(callCC))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Data.Aeson (FromJSON, decodeStrict, parseJSON, withObject, (.:))
import Data.ByteString.Char8 (pack)
import Data.Char (isSpace)
import Data.List (intersperse, sort, sortOn)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Vector (Vector, fromList, imap, (!), (!?))
import qualified Data.Vector as V (find, map)
import System.IO (IOMode (ReadMode), hSetBuffering, readFile, hFlush, stdout, stdin, BufferMode (NoBuffering))
import System.IO.NoBufferingWorkaround (initGetCharNoBuffering, getCharNoBuffering)
import Data.Ord (Down(Down))
import System.Info (os)

import Data.WordTree

-- | Used Keyboard, the only one supported for now is QWERTY
type Keyboard = Vector Char

instance FromJSON CountedWord where
  parseJSON = withObject "CountedWord" $ \v ->
    CountedWord
      <$> v .: "word"
      <*> v .: "properties"

-- | Entrypoint into the spell-checker
main :: IO ()
main = do
  if os == "mingw32" then initGetCharNoBuffering >> hSetBuffering stdout NoBuffering
  else hSetBuffering stdin NoBuffering
  contents <- readFile "app//Statistics//result.txt"
  let inputFreq = words contents
  let dictionaryTree = listToTree $ mapMaybe (decodeStrict . pack) inputFreq
  putStrLn "Type enter to correct a word or tab to complete it."
  putStrLn "Type a word:"
  (`runContT` return) $ do
    callCC $ prompt dictionaryTree

-- | Prompts a new word as long as the word to correct is not empty
prompt :: Tree Char -> (() -> ContT r IO ()) -> ContT r IO ()
prompt tree exit = do
  restart <- label_
  query <- liftIO $ putStr "> " >> hFlush stdout >> getWord
  case query of
    Complete word -> liftIO $ do
      putStrLn ""
      print $ completeWord tree word
    Correct "" -> exit ()
    Correct word -> liftIO $ print $ correctWord tree word
  restart

-- | The user can either ask for completion or correction
data Query a = Complete a | Correct a deriving Functor

-- | Give the input of the user
getWord :: IO (Query String)
getWord = do
  c <- if os == "mingw32" then getCharNoBuffering
       else getChar
  when (os == "mingw32") $ putChar c
  case c of
    '\t' -> return $ Complete []
    _ | isSpace c -> do
          when (os == "mingw32") $ putChar '\n'
          return $ Correct []
      | otherwise -> fmap (c:) <$> getWord

-- | Complete user's input
completeWord :: Tree Char -> String -> [CountedWord]
completeWord tree prefixe = take 10 . sortOn Down $ giveSuffixe tree prefixe

-- | Correct user's input
correctWord :: Tree Char -> String -> [CountedWord]
correctWord tree word = take 10 . sortOn (\x -> (strDiff (CountedWord word nullProperties) x, x)) $ similarWords tree 2 word

-- | [Not used for now] Correct a whole line, giving only one answer
correctLine :: Tree Char -> String -> CountedWord
correctLine tree line = assemble correctWords
  where
    correctWords = map (head . correctWord tree) $ words line
    assemble = foldl (\w1 w2 -> CountedWord (show w1 ++ " " ++ show w2) (WordProperties (negate 1) []))
                      (CountedWord "" nullProperties)

-- | QWERTY Keyboard used to get neighboors leters from the on typed 
keyboardEn :: Keyboard
keyboardEn =
  fromList
    [ 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p',
      'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';',
      'z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.'
    ]

-- | Give the zone of index of near characters from the index of the one given
nearIndices :: Integral a => a -> [a]
nearIndices ind = case ind `mod` 10 of
  0 -> [ind + 1, ind + 10, ind - 10, ind - 9, ind + 11]
  9 -> [ind - 1, ind + 10, ind - 10, ind + 9, ind - 11]
  _ -> [ind - 1, ind + 1, ind + 10, ind - 10, ind - 11, ind + 11, ind - 9, ind + 9]

-- | Give the matrix of neighboorhood in place of the character of the keyboard 
charsPerimeter :: Keyboard -> Vector [Char]
charsPerimeter keyboard = imap (\ind _ -> mapMaybe (keyboard !?) $ nearIndices ind) keyboard

-- | Given a keyboard and a perimeter, associate between each characters and his neighboors
associateNearChars :: Keyboard -> Vector [Char] -> Vector (Char, [Char])
associateNearChars keyboard perimeter = imap (\ind char -> (char, perimeter ! ind)) keyboard

-- | Gives the neighboors of all characters
nearChars :: Keyboard -> Vector (Char, [Char])
nearChars keyboard = associateNearChars keyboard $ charsPerimeter keyboard

-- | The keyboard we choose to use
actualKeyboard :: Vector (Char, [Char])
actualKeyboard = nearChars keyboardEn

-- | Calcul of the distance between two words (Hamming's distance modifed)
strDiff :: CountedWord -> CountedWord -> Int
strDiff (CountedWord x _) (CountedWord [] _) = length x
strDiff (CountedWord [] _) (CountedWord y _) = length y
strDiff wx@(CountedWord (x : xs) freqx) wy@(CountedWord (y : ys) freqy) =
  if x == y
    then strDiff (CountedWord xs freqx) (CountedWord ys freqy)
    else 2 + diffMin
  where
    diffMin = min diffMinq $ min (strDiff (CountedWord xs freqx) wy) (strDiff wx (CountedWord ys freqy))
    -- Case where they are "near" :
    diffMinq =
      strDiff (CountedWord xs freqx) (CountedWord ys freqy)
        - if elem x $ nearChar y
          then 1
          else 0
    nearChar c = maybe [] snd $ V.find (\z -> c == fst z) actualKeyboard
