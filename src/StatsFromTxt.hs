{-# LANGUAGE DataKinds, TypeOperators, DeriveGeneric, OverloadedStrings #-}

-- --------------------------------------------------------------------------
-- |
-- Module      :  StatsFromTxt
--
--
-- A module to construct statistical data out of text files.
--
-----------------------------------------------------------------------------

module Main (main) where

import Control.Monad (forM)
import Codec.Serialise (writeFileSerialise)
import Options.Generic (getRecord, ParseRecord, Generic, type (<?>))

import Data.List (insertBy, find, sortOn)
import Data.Maybe (maybe, fromMaybe, listToMaybe)
import Data.Bifunctor (second)
import qualified Data.Map.Strict as M
  (Map, empty, insertWith, foldrWithKey, map)

import System.IO (readFile)
import System.Directory (listDirectory)

-- Should we serialize the Tree instead of the Map? --

newtype Arguments = Arguments
    { language :: String    <?> "Language of the dictionary to do the computation (e.g en)"
    } deriving (Generic)

instance ParseRecord Arguments

main :: IO()
main = do
  lang <- getRecord "Program to compute statistical data out of dictionaries"
  serializeMap lang

-- | Fetch words from a file
getWords :: FilePath -> IO [String]
getWords file = words <$> readFile file

-- | Assemble all words from different dictionaries
getFiles :: String -> IO [String]
getFiles lang = do
  let dictionaryFiles = "Dictionaries//" ++ lang
  files <- map ((dictionaryFiles ++ "//") ++) <$> listDirectory dictionaryFiles
  wordsPerFile <- forM files getWords
  return $ concat wordsPerFile

insertDown :: (Int, String) -> [(Int, String)] -> [(Int, String)]
insertDown = insertBy (\a b -> compare b a)

-- | Merge two same words by suming the frequencies and following words 
-- to the ones already recorded 
addValue :: (Int, [(Int, String)]) -> (Int, [(Int, String)]) -> (Int, [(Int, String)])
addValue (_, [(_, nextWord)]) (freq, words) =
  (freq + 1, filter ((/= nextWord) . snd) $ insertDown (nbSeen, nextWord) words)
  where nbSeen = 1 + maybe 0 fst (find ((== nextWord) . snd) words)
  -- ^ nbSeen is the number of occurences of nextWord following the current word

-- | Map a word to his frequence and the next words with the probabilities associated
getFreqnNext :: [String] -> M.Map String (Int, [(Int, String)])
getFreqnNext [] = M.empty
getFreqnNext (w:ws) = M.insertWith addValue w v $ getFreqnNext ws
        where v = (1, [(1, fromMaybe "" $ listToMaybe ws)])

-- | Function to get the following words in sorted order
getResultingMap :: M.Map String (Int, [(Int, String)]) -> M.Map String (Int, [String])
getResultingMap = M.map $ second $ map snd . take 3

-- | Fetch statistics as a Map object
getStatsFromFile :: String -> IO (M.Map String (Int, [String]))
getStatsFromFile lang = getResultingMap.getFreqnNext <$> getFiles lang

-- | Transforms a Map storing statistics of words to stringified JSON
mapToStr :: M.Map String (Int, [String]) -> String
mapToStr = M.foldrWithKey (\key value str -> translateWord key value ++ str) ""
  where translateWord word (freq, nextWords) = "{\"word\":" ++ show word
                                                  ++ ",\"properties\":[" ++ show freq
                                                  ++ (',' : show (take 3 nextWords)) ++ "]} "

-- | Serialize the map associating words to their properties in a file
serializeMap :: String -> IO ()
serializeMap lang = writeFileSerialise "SerializedStatistics/result" =<< getStatsFromFile lang
