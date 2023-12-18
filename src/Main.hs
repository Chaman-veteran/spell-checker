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

module Main (main) where

import Control.Monad.Cont (ContT(runContT), label_, MonadCont(callCC))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when, join)

import Data.Char (isSpace)
import Data.List (sort, sortOn)
import Data.Maybe (mapMaybe, fromJust, catMaybes)
import Data.Vector (Vector, imap, (!), (!?))
import Data.Aeson (decode, Array, Value(Object, Array, String))
import Data.Ord (Down(Down))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson.KeyMap as KM ((!?), Key)
import qualified Data.Vector as V (find, map, fromList, empty, zipWith, zip, concat, toList)
import qualified Data.ByteString.Lazy as B (readFile)

import System.IO (IOMode (ReadMode), hSetBuffering,
    hFlush, stdout, stdin, BufferMode (NoBuffering))
import System.IO.NoBufferingWorkaround (initGetCharNoBuffering, getCharNoBuffering)
import System.Info (os)
import Codec.Serialise (readFileDeserialise)

import Data.WordTree

-- | Used Keyboard, the only one supported for now is QWERTY
type Keyboard = Vector (Vector Text)

-- | Entrypoint into the spell-checker
main :: IO ()
main = do
  if os == "mingw32" then initGetCharNoBuffering >> hSetBuffering stdout NoBuffering
  else hSetBuffering stdin NoBuffering
  inputFreq <- readFileDeserialise "SerializedStatistics/result"
  let dictionaryTree = fromMap inputFreq
  putStrLn "Type enter to correct a word or tab to complete it."
  putStrLn "Type a word:"
  (`runContT` return) $ callCC $ prompt dictionaryTree

-- | The user can either ask for completion or correction
data Query a = Complete a | Correct a deriving Functor

-- | Gives the input of the user
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
    _ -> fmap (c:) <$> getWord

-- | Prompts a new word as long as the word to correct is not empty
prompt :: Tree Char -> (() -> ContT r IO ()) -> ContT r IO ()
prompt tree exit = do
  restart <- label_
  query <- liftIO $ putStr "> " >> hFlush stdout >> getWord
  case query of
    Complete word -> liftIO $ do
      putStrLn ""
      print $ completeWord tree $ T.pack word
    Correct "" -> exit ()
    Correct word -> liftIO $ print $ correctWord tree word
  restart

-- | Complete user's input
completeWord :: Tree Char -> Text -> [CountedWord Text]
completeWord tree prefixe = take 10 . sortOn Down $ giveSuffixe tree prefixe

associateDistance :: Text -> CountedWord Text -> (Int, CountedWord Text)
associateDistance word x = (strDiff (CountedWord word nullProperties) x, x)

-- | Correct user's input
correctWord :: Tree Char -> String -> [CountedWord Text]
correctWord tree word = take 10 . sortOn (associateDistance $ T.pack word) $ similarWords tree 2 word

-- | [Not used for now] Correct a whole line, giving only one answer
correctLine :: Tree Char -> String -> CountedWord Text
correctLine tree line = assemble correctWords
  where
    correctWords = map (head . correctWord tree) $ words line
    assemble = foldl (\w1 w2 -> CountedWord (word w1 `T.snoc` ' ' `T.append` word w2) (WordProperties (negate 1) []))
                      (CountedWord "" nullProperties)

-- | Extracts the text value out of a json
getFromJSON :: KM.Key -> Value -> Text
getFromJSON key (Object o) = (\(String s) -> s) . fromJust $ o KM.!? key

-- | QWERTY Keyboard used to get neighboors leters from the on typed
keyboardEn :: IO Keyboard
keyboardEn = do
  layoutB <- B.readFile "layouts/qwerty.json"
  let layoutArray = fromJust (decode layoutB :: Maybe Array)
  let fromJSON = V.map (\(Array v) -> V.map (getFromJSON "label") v) layoutArray
  return fromJSON

-- | Gives the zone of index of near characters from the index of the one given
nearIndices :: (Num a, Num b, Eq a, Eq b) => a -> b -> [(a, b)]
nearIndices indR indT = [(row, text) | row <- [indR - 1, indR, indR + 1],
                                        text <- [indT - 1, indT, indT + 1],
                                        (row, text) /= (indR, indT)]

accessMatrix :: Keyboard -> (Int, Int) -> Maybe Text
accessMatrix matrix (row, column) = (!? column) =<< (matrix !? row)

-- | Give the matrix of neighboorhood in place of the character of the keyboard 
charsPerimeter :: Keyboard -> Vector (Vector [Text])
charsPerimeter keyboard = imap (\indR row -> imap
                                              (\indT _ -> mapMaybe (accessMatrix keyboard) $ nearIndices indR indT)
                                              row)
                                keyboard

-- | Given a keyboard and a perimeter, associate between each characters and his neighboors
associateNearChars :: Keyboard -> Vector (Vector [Text]) -> Vector (Text, [Text])
associateNearChars keyboard perimeter = V.concat . V.toList $ V.zipWith V.zip keyboard perimeter

-- | Gives the neighboors of all characters
nearChars :: Keyboard -> Vector (Text, [Text])
nearChars keyboard = associateNearChars keyboard $ charsPerimeter keyboard

-- | The keyboard we choose to use
actualKeyboard :: IO (Vector (Text, [Text]))
actualKeyboard = nearChars <$> keyboardEn

inPerimeterOf :: Text -> IO [Text]
inPerimeterOf c = maybe [] snd . V.find (\z -> c == fst z) <$> actualKeyboard

-- | Calcul of the distance between two words (modifed Hamming's distance)
--strDiff :: CountedWord -> CountedWord -> Int
--strDiff (CountedWord x _) (CountedWord [] _) = length x
--strDiff (CountedWord [] _) (CountedWord y _) = length y
-- strDiff wx@(CountedWord (x : xs) freqx) wy@(CountedWord (y : ys) freqy) =
--   do
--     perimeterOfy <- inPerimeterOf y
--     if x == y
--       then strDiff (CountedWord xs freqx) (CountedWord ys freqy)
--       else 2 + diffMin
--   where
--     diffMin = min diffMinq $ min (strDiff (CountedWord xs freqx) wy) (strDiff wx (CountedWord ys freqy))
--     -- Case where they are "near" :
--     diffMinq =
--       strDiff (CountedWord xs freqx) (CountedWord ys freqy)
--         - if x `elem` perimeterOfy
--           then 1
--           else 0
strDiff _ _ = 0
