{-# LANGUAGE OverloadedStrings #-}

-- --------------------------------------------------------------------------
-- |
-- Module      :  SpellCheckerInterface
--
--
--  Spell-checker main API, exporting functions s.a. completeWord or correctWord.
--
-----------------------------------------------------------------------------

module SpellCheckerInterface (completeWord, correctWord, getKeyboardEn ) where

import Data.Maybe (mapMaybe, fromJust)
import Data.Ord (Down(Down))
import Data.List (sortOn)
import Data.Vector (Vector, imap, (!?))
import Data.Aeson (decode, Array, Value(Object, Array, String))
import qualified Data.Text as T (head)
import qualified Data.ByteString.Lazy as B (readFile)
import qualified Data.Vector as V (find, map, zip, zipWith, concat, toList)
import qualified Data.Aeson.KeyMap as KM ((!?), Key)

import Control.Monad.Reader (Reader, ask)

import Data.WordTree

-- | Used Keyboard, the only one supported for now is QWERTY
type Keyboard = Vector (Vector Char)


-- | Complete user's input
completeWord :: Tree Char -> String -> [CountedWord]
completeWord tree prefixe = take 10 . sortOn Down $ giveSuffixe tree prefixe


-- | Correct user's input
correctWord :: Tree Char -> String -> WithKeyboard [CountedWord]
correctWord tree word = do
    wordsWithDistance <- mapM (associateDistance word) $ similarWords tree 2 word
    return $ take 10 . map snd $ sortOn fst wordsWithDistance

-- | [Unused for now] Correct a whole line, giving only one answer
-- correctLine :: Tree Char -> String -> CountedWord
-- correctLine tree line = assemble correctWords
--   where
--     correctWords = map (head <$> correctWord tree) $ words line
--     assemble = foldl (\w1 w2 -> CountedWord (show w1 ++ " " ++ show w2) (WordProperties (negate 1) []))
--                       (CountedWord "" nullProperties)

-- | Extracts the text value out of a json
getFromJSON :: KM.Key -> Value -> Char
getFromJSON key (Object o) = (\(String s) -> T.head s) . fromJust $ o KM.!? key

-- | QWERTY Keyboard used to get neighboors leters from the on typed
getKeyboardEn :: IO Keyboard
getKeyboardEn = do
  layoutB <- B.readFile "layouts/qwerty.json"
  let layoutArray = fromJust (decode layoutB :: Maybe Array)
  let fromJSON = V.map (\(Array v) -> V.map (getFromJSON "label") v) layoutArray
  return fromJSON

-- | Gives the zone of index of near characters from the index of the one given
nearIndices :: (Num a, Num b, Eq a, Eq b) => a -> b -> [(a, b)]
nearIndices indR indT = [(row, text) | row <- [indR - 1, indR, indR + 1],
                                        text <- [indT - 1, indT, indT + 1],
                                        (row, text) /= (indR, indT)]

-- | Function to access an element of a matrix given it's (row, column) coordinates
accessMatrix :: Keyboard -> (Int, Int) -> Maybe Char
accessMatrix matrix (row, column) = (!? column) =<< (matrix !? row)

-- | Give the matrix of neighboorhood in place of the character of the keyboard 
charsPerimeter :: Keyboard -> Vector (Vector [Char])
charsPerimeter keyboard = imap (\indR row -> imap
                                              (\indT _ -> mapMaybe (accessMatrix keyboard) $ nearIndices indR indT)
                                              row)
                                keyboard

-- | Given a keyboard and a perimeter, associate between each characters and his neighbors
associateNearChars :: Keyboard -> Vector (Vector [Char]) -> Vector (Char, [Char])
associateNearChars keyboard perimeter = V.concat . V.toList $ V.zipWith V.zip keyboard perimeter

-- | Gives the neighboors of all characters
nearChars :: Keyboard -> Vector (Char, [Char])
nearChars keyboard = associateNearChars keyboard $ charsPerimeter keyboard

type WithKeyboard = Reader Keyboard

associateDistance :: String -> CountedWord -> WithKeyboard (Int, CountedWord)
associateDistance w x = do
    diff <- strDiff w (word x)
    return (diff, x)

-- | The keyboard we choose to use
actualKeyboard :: WithKeyboard (Vector (Char, [Char]))
actualKeyboard = do
  keyboard <- ask
  return $ nearChars keyboard

-- | Returns the perimeter of a character, as per nearChars's definition of "neighbor"
inPerimeterOf :: Char -> WithKeyboard [Char]
inPerimeterOf c = (maybe [] snd . V.find ((c == ) . fst)) <$> actualKeyboard

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
