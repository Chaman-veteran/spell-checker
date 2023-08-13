module StatsFromTxt where

import System.IO (readFile, writeFile)
import System.Directory (listDirectory)
import qualified Data.Map.Strict as M
  (Map, insert, empty, insertWith, foldrWithKey, map)
import Control.Monad (forM)
import Data.List (insert, find, sortOn)
import Data.Maybe (maybe)
import Data.Bifunctor (second)

-- TODO : Serialize the Map (or the Tree?) instead of the Map as a String
-- see : https://hackage.haskell.org/package/serialise

-- | Fetch words from a file
getWords :: FilePath -> IO [String]
getWords file = words <$> readFile ("Dictionaries//"++file)

-- | Assemble all words from different dictionaries
getFiles :: IO [String]
getFiles = do
  files <- listDirectory "Dictionaries"
  wordsPerFile <- forM files getWords
  return $ concat wordsPerFile

-- | Merge two same words by suming the frequencies and following words 
addValue :: (Int, [(String,Int)]) -> (Int, [(String,Int)]) -> (Int, [(String,Int)])
addValue (incFreq, [(nextWord,_)]) (freq, words) = (freq+incFreq, insert (nextWord, pNext) words)
  where pNext = 1 + maybe 0 snd (find (\w -> fst w == nextWord) words)

-- | Map a word to his frequence and the next words with the probabilities associated
getFreqnNext :: [String] -> M.Map String (Int, [(String,Int)])
getFreqnNext [] = M.empty
getFreqnNext [word] = M.insertWith addValue word (1, [(".",1)]) M.empty
getFreqnNext (w:ws) = M.insertWith addValue w (1, [(head ws,1)]) $ getFreqnNext ws

-- | Function to get the following words in sorted order
getNextsSorted :: M.Map String (Int, [(String, Int)]) -> M.Map String (Int, [String])
getNextsSorted = M.map $ second (map fst . sortOn snd)

-- | Fetch statistics as a Map object
getStatsFromFile :: IO (M.Map String (Int, [String]))
getStatsFromFile = getNextsSorted.getFreqnNext <$> getFiles

-- | Transforms a Map storing statistics of words to stringified JSON
mapToStr :: M.Map String (Int, [String]) -> String
mapToStr = M.foldrWithKey (\key value str -> translateWord key value ++ str) "" 
  where translateWord word (freq, nextWords) = "{\"word\":"++ show word
                                                  ++",\"properties\":["++show freq
                                                  ++","++show (take 3 nextWords)++"]} "

-- | Writes statistics (as stringified JSON) WordProperties
-- from files in Dictionaries to Statistics/result.txt
dictToStats :: IO ()
dictToStats = do
  stats <- getStatsFromFile
  writeFile "Statistics/result.txt" $ mapToStr stats
