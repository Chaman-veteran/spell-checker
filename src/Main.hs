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
import Control.Monad (when)

import Data.Char (isSpace)
import Data.List (sort, sortOn)
import Data.Maybe (mapMaybe, fromJust)
import Data.Ord (Down(Down))
import Data.Vector (Vector, imap, (!), (!?))
import Data.Aeson (decode, Array, Value(Object, Array, String))
import qualified Data.Aeson.KeyMap as KM ((!?), Key)
import qualified Data.Vector as V (find, map, fromList, zip, zipWith, concat, toList)
import qualified Data.ByteString.Lazy as B (readFile)
import qualified Data.Text as T (head)

import System.IO (IOMode (ReadMode), hSetBuffering,
    hFlush, stdout, stdin, BufferMode (NoBuffering))
import System.IO.NoBufferingWorkaround (initGetCharNoBuffering, getCharNoBuffering)
import System.Info (os)
import Codec.Serialise (readFileDeserialise)

import Data.WordTree

-- | Used Keyboard, the only one supported for now is QWERTY
type Keyboard = Vector (Vector Char)

-- | Entrypoint into the spell-checker
main :: IO ()
main = do
  if os == "mingw32" then initGetCharNoBuffering >> hSetBuffering stdout NoBuffering
  else hSetBuffering stdin NoBuffering
  inputFreq <- readFileDeserialise "SerializedStatistics/result" 
  let dictionaryTree = fromMap inputFreq
  putStrLn "Type enter to correct a word or tab to complete it."
  putStrLn "Type a word:"
  (`runContT` return) $ do
    callCC $ prompt dictionaryTree

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
    _ -> fmap (c:) <$> getWord

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
    Correct word -> liftIO $ print =<< correctWord tree word
  restart

-- | Complete user's input
completeWord :: Tree Char -> String -> [CountedWord]
completeWord tree prefixe = take 10 . sortOn Down $ giveSuffixe tree prefixe

associateDistance :: String -> CountedWord -> IO (Int, CountedWord)
associateDistance w x = do
    distance <- strDiff w (word x)
    return (distance, x)

-- | Correct user's input
correctWord :: Tree Char -> String -> IO [CountedWord]
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

accessMatrix :: Keyboard -> (Int, Int) -> Maybe Char
accessMatrix matrix (row, column) = (!? column) =<< (matrix !? row)

-- | Give the matrix of neighboorhood in place of the character of the keyboard 
charsPerimeter :: Keyboard -> Vector (Vector [Char])
charsPerimeter keyboard = imap (\indR row -> imap
                                              (\indT _ -> mapMaybe (accessMatrix keyboard) $ nearIndices indR indT)
                                              row)
                                keyboard

-- | Given a keyboard and a perimeter, associate between each characters and his neighboors
associateNearChars :: Keyboard -> Vector (Vector [Char]) -> Vector (Char, [Char])
associateNearChars keyboard perimeter = V.concat . V.toList $ V.zipWith V.zip keyboard perimeter

-- | Gives the neighboors of all characters
nearChars :: Keyboard -> Vector (Char, [Char])
nearChars keyboard = associateNearChars keyboard $ charsPerimeter keyboard

-- | The keyboard we choose to use
actualKeyboard :: IO (Vector (Char, [Char]))
actualKeyboard = nearChars <$> keyboardEn

inPerimeterOf :: Char -> IO String
inPerimeterOf c = maybe [] snd . V.find ((c == ) . fst) <$> actualKeyboard

min' :: IO Int -> IO Int -> IO Int
min' a b = min <$> a <*> b

-- | Calcul of the distance between two words (modifed Hamming's distance)
strDiff :: String -> String -> IO Int
strDiff x [] = return $ length x
strDiff [] y = return $ length y
strDiff (x : xs) (y : ys) =
    if x == y then
      strDiff xs ys
    else do
      perimeterOfy <- inPerimeterOf y
      (+2) <$> diffMinq perimeterOfy
  where
    diffMin perimeterOfy = min' (diffMinq perimeterOfy) $ min' (strDiff xs (y:ys)) (strDiff (x:xs) ys)
    -- Case where they are "near" :
    diffMinq perimeterOfy = do
        distance <- strDiff xs ys
        return $ distance - fromEnum (x `elem` perimeterOfy)
