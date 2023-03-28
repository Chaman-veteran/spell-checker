{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE OverloadedStrings #-}

-- Thanks for archive.org to find free books in txt format --

-- TODO LIST --
-- See todo in StatsFromTxt
-- Writing the predictive part of the spell-checker

module Main (main) where
import System.IO ( readFile, IOMode(ReadMode), hSetBuffering )
import Data.List ( intersperse, sortOn, sort )
import Data.Vector ( Vector, fromList, imap, (!?), (!) )
import qualified Data.Vector as V ( map, find )
import Data.Aeson ( FromJSON, parseJSON, withObject, (.:), decodeStrict )
import Data.ByteString.Char8 ( pack ) 
import Data.Maybe ( mapMaybe )
import WordsTrees
--import Control.Parallel ( par, pseq )

type Keyboard = Vector Char

instance FromJSON CountedWords where
    parseJSON = withObject "CountedWords" $ \v -> CountedWords
        <$> v .: "word"
        <*> v .: "properties"

main :: IO ()
main = do
    contents <- readFile "app//Statistics//result.txt"
    let inputFreq = words contents
    let dictionaryTree = listToTree $ mapMaybe (decodeStrict.pack) inputFreq
    putStrLn "Type enter to correct a word or tab + enter to complete the current word."
    putStrLn "Type a word:"
    prompt dictionaryTree
      where prompt tree =
              do putStr "> "
                 line <- getLine
                 if null line
                   then return ()
                 else do print $ if last line == '\t' then completeWord tree $ init line
                                 else correctWord tree line
                         prompt tree

completeWord :: Tree Char -> String -> [CountedWords]
completeWord tree prefixe = take 10 . sort $ giveSuffixe tree prefixe 

correctWord :: Tree Char -> String -> [CountedWords]
correctWord tree word = take 10 . sortOn (\x -> (strDiff (CountedWords word (0,[])) x, x)) $ similarWords tree 2 word

correctLine :: Tree Char -> String -> CountedWords
correctLine tree line = assemble correctWords
    where correctWords = map (head . correctWord tree) $ words line
          assemble = foldl (\w1 w2 -> CountedWords (show w1 ++ " " ++ show w2) (-1,[])) (CountedWords "" (-1, []))

prettyPrint :: [String] -> IO ()
prettyPrint l = mapM_ putStr $ ["["] ++ intersperse ", " l ++ ["]\n"]

keyboardEn :: Keyboard
keyboardEn = fromList 
        [ 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p'
        , 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';'
        , 'z', 'x', 'c', 'v', 'b', 'n', 'm',',','.']

-- Define the zone of near characters
nearIndices :: Integral a => a -> [a]
nearIndices ind = case ind `mod` 10 of
    0 -> [ind+1, ind+10, ind-10, ind-9, ind+11]
    9 -> [ind-1, ind+10, ind-10, ind+9, ind-11]
    _ -> [ind-1, ind+1, ind+10, ind-10, ind-11, ind+11, ind-9, ind+9]

-- Gives near chars for each one on the keyboard
charsPerimeter :: Keyboard -> Vector [Char]
charsPerimeter keyboard = imap (\ind _ -> mapMaybe (keyboard!?) $ nearIndices ind) keyboard

-- Association between chars and his neighboors
associateNearChars :: Keyboard -> Vector [Char] -> Vector (Char, [Char])
associateNearChars keyboard perimeter = imap (\ind char -> (char, perimeter!ind)) keyboard 

-- Gives the neighboors of all chars
nearChars :: Keyboard -> Vector (Char, [Char])
nearChars keyboard = associateNearChars keyboard $ charsPerimeter keyboard

-- Used keyboard
actualKeyboard :: Vector (Char, [Char])
actualKeyboard = nearChars keyboardEn

outMaybeAssocList :: Maybe (a, [b]) -> [b]
outMaybeAssocList Nothing = []
outMaybeAssocList (Just(_,l)) = l

-- Calcul of the distance between two words (Hamming's distance modifed)
strDiff :: CountedWords -> CountedWords -> Int
strDiff (CountedWords x _) (CountedWords [] _) = length x
strDiff (CountedWords [] _) (CountedWords y _) = length y
strDiff wx@(CountedWords (x:xs) freqx) wy@(CountedWords (y:ys) freqy) =
    if x==y then strDiff (CountedWords xs freqx) (CountedWords ys freqy)
    else 2 + diffMin
        where diffMin = min diffMinq $ min (strDiff (CountedWords xs freqx) wy) (strDiff wx (CountedWords ys freqy))
              -- Case where they are "near" :
              diffMinq = strDiff (CountedWords xs freqx) (CountedWords ys freqy)
                      - if elem x $ nearChar y then 1
                        else 0
              nearChar c = outMaybeAssocList $ V.find (\z -> c == fst z) actualKeyboard
