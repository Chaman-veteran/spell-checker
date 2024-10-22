{-# LANGUAGE OverloadedStrings #-}

-- --------------------------------------------------------------------------
-- |
-- Module      :  Keyboard
--
-- Module fetching the keyboard to be used from a json file.
-----------------------------------------------------------------------------

module Data.Keyboard (WithKeyboard, actualKeyboard) where

import Data.Maybe (fromJust)
import Data.Vector (Vector, imap, (!?))
import Data.Maybe (mapMaybe, fromJust)
import Data.Aeson (decode, Array, Value(Object, Array, String))
import qualified Data.ByteString.Lazy as B (readFile)
import qualified Data.Vector as V (map, zip, zipWith, concat, toList)
import qualified Data.Aeson.KeyMap as KM ((!?), Key)
import qualified Data.Text as T (head)

import Control.Monad.Reader (Reader)

-- | Used Layout, the only one supported for now is QWERTY
type Layout = Vector (Vector Char)
-- | A Keyboard is an association of a layout with, for each letter the associated neighbors
type Keyboard = Vector (Char, [Char])

type WithKeyboard = Reader Keyboard

-- | Gives the zone of index of near characters from the index of the one given
nearIndices :: (Num a, Num b, Eq a, Eq b) => a -> b -> [(a, b)]
nearIndices indR indT = [(row, text) | row <- [indR - 1, indR, indR + 1],
                                        text <- [indT - 1, indT, indT + 1],
                                        (row, text) /= (indR, indT)]

-- | Function to access an element of a matrix given it's (row, column) coordinates
accessMatrix :: Layout -> (Int, Int) -> Maybe Char
accessMatrix matrix (row, column) = (!? column) =<< (matrix !? row)

-- | Extracts the text value out of a json
getFromJSON :: KM.Key -> Value -> Char
getFromJSON key (Object o) = (\(String s) -> T.head s) . fromJust $ o KM.!? key

-- | QWERTY Layout used to get neighboors leters from the on typed
getKeyboardEn :: IO Layout
getKeyboardEn = do
  layoutB <- B.readFile "layouts/qwerty.json"
  let layoutArray = fromJust (decode layoutB :: Maybe Array)
  let fromJSON = V.map (\(Array v) -> V.map (getFromJSON "label") v) layoutArray
  return fromJSON

-- | Give the matrix of neighboorhood in place of the character of the keyboard 
charsPerimeter :: Layout -> Vector (Vector [Char])
charsPerimeter keyboard = imap (\indR row -> imap
                                              (\indT _ -> mapMaybe (accessMatrix keyboard) $ nearIndices indR indT)
                                              row)
                                keyboard

-- | Given a keyboard and a perimeter, associate between each characters and his neighbors
associateNearChars :: Layout -> Vector (Vector [Char]) -> Keyboard
associateNearChars keyboard perimeter = V.concat . V.toList $ V.zipWith V.zip keyboard perimeter

-- | Gives the neighboors of all characters
nearChars :: Layout -> Keyboard
nearChars keyboard = associateNearChars keyboard $ charsPerimeter keyboard

-- | The keyboard we choose to use
actualKeyboard :: IO Keyboard
actualKeyboard = nearChars <$> getKeyboardEn
