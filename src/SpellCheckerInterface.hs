{-# LANGUAGE OverloadedStrings #-}

-- --------------------------------------------------------------------------
-- |
-- Module      :  SpellCheckerInterface
--
--
--  Spell-checker main API, exporting functions s.a. completeWord or correctWord.
--
-----------------------------------------------------------------------------

module SpellCheckerInterface (completeWord, correctWord) where

import Data.Ord (Down(Down))
import Data.List (sortOn)
import Data.Vector (Vector)
import qualified Data.Vector as V (find)

import Control.Monad.Reader (ask)

import Data.WordTree (giveSuffixe, similarWords, CountedWord(word), Tree)
import Data.Keyboard (WithKeyboard, actualKeyboard)

-- | Complete user's input
completeWord :: Tree Char -> String -> [CountedWord]
completeWord tree prefixe = take 10 . sortOn Down $ giveSuffixe tree prefixe

-- | Correct user's input
correctWord :: Tree Char -> String -> WithKeyboard [CountedWord]
correctWord tree word = do
    wordsWithDistance <- mapM (associateDistance word) $ similarWords tree 2 word
    return $ take 10 . map snd $ sortOn fst wordsWithDistance

associateDistance :: String -> CountedWord -> WithKeyboard (Int, CountedWord)
associateDistance w x = do
    diff <- strDiff w (word x)
    return (diff, x)

-- | Returns the perimeter of a character, as per nearChars's definition of "neighbor"
inPerimeterOf :: Char -> WithKeyboard [Char]
inPerimeterOf c = (maybe [] snd . V.find ((c == ) . fst)) <$> ask

-- | Calcul of the distance between two words (modifed Hamming's distance)
strDiff :: String -> String -> WithKeyboard Int
strDiff x "" = return $ length x
strDiff "" y = return $ length y
strDiff (x : xs) (y : ys) | x == y = strDiff xs ys
strDiff (x : xs) (y : ys) =
    let perimeterDiff = do
          perimeterOfy <- inPerimeterOf y
          tailDiff <- strDiff xs ys
          return $ tailDiff - fromEnum (x `elem` perimeterOfy) in
    ((2 +) . minimum) <$> sequence [perimeterDiff, strDiff xs (y:ys), strDiff (x:xs) ys]
