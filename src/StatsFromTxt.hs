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

import System.IO (readFile, writeFile)
import System.Directory (listDirectory)
import qualified Data.Map.Strict as M
  (Map, insert, empty, insertWith, foldrWithKey, map)
import Control.Monad (forM)
import Data.List (insert, find, sortOn)
import Data.Maybe (maybe)
import Data.Bifunctor (second)
import Codec.Serialise (writeFileSerialise)
import Options.Generic (getRecord, ParseRecord, Generic, type (<?>))

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

-- | Merge two same words by suming the frequencies and following words 
addValue :: (Int, [(String,Int)]) -> (Int, [(String,Int)]) -> (Int, [(String,Int)])
addValue (incFreq, [(nextWord,_)]) (freq, words) = (freq+incFreq, insert (nextWord, pNext) words)
  where pNext = 1 + maybe 0 snd (find ((== nextWord) . fst) words)

-- | Map a word to his frequence and the next words with the probabilities associated
getFreqnNext :: [String] -> M.Map String (Int, [(String,Int)])
getFreqnNext [] = M.empty
getFreqnNext [word] = M.insertWith addValue word (1, [(".",1)]) M.empty
getFreqnNext (w:ws) = M.insertWith addValue w (1, [(head ws,1)]) $ getFreqnNext ws

-- | Function to get the following words in sorted order
getNextsSorted :: M.Map String (Int, [(String, Int)]) -> M.Map String (Int, [String])
getNextsSorted = M.map $ second (map fst . sortOn snd)

-- | Fetch statistics as a Map object
getStatsFromFile :: String -> IO (M.Map String (Int, [String]))
getStatsFromFile lang = getNextsSorted.getFreqnNext <$> getFiles lang

-- | Transforms a Map storing statistics of words to stringified JSON
mapToStr :: M.Map String (Int, [String]) -> String
mapToStr = M.foldrWithKey (\key value str -> translateWord key value ++ str) ""
  where translateWord word (freq, nextWords) = "{\"word\":" ++ show word
                                                  ++ ",\"properties\":[" ++ show freq
                                                  ++ (',' : show (take 3 nextWords)) ++ "]} "

-- | Serialize the map associating words to their properties in a file
serializeMap :: String -> IO ()
serializeMap lang = writeFileSerialise "SerializedStatistics/result" . M.map (second (take 3)) =<< getStatsFromFile lang
