module StatsFromTxt where
import System.IO ( readFile, writeFile )
import System.Directory ( listDirectory )
import qualified Data.Map.Strict as Map
  ( Map, insert, empty, insertWith, foldrWithKey, map )
import Control.Monad ( forM )
import Data.List ( insert, find, sortOn )
import Data.Maybe ( maybe )
import Data.Bifunctor ( second )

-- TODO : Serialize the Map (or the Tree?) instead of the Map as a String
-- see : https://hackage.haskell.org/package/serialise

getWords :: FilePath -> IO [String]
getWords file = words <$> readFile ("Dictionnaries//"++file)

getFiles :: IO [String]
getFiles = do
  files <- listDirectory "Dictionnaries"
  wordsPerFile <- forM files getWords
  return $ concat wordsPerFile

-- Error : addValue not exhaustive
addValue :: (Int, [(String,Int)]) -> (Int, [(String,Int)]) -> (Int, [(String,Int)])
addValue (incFreq, [(nextWord,_)]) (freq, words) = (freq+incFreq, insert (nextWord, pNext) words)
  where pNext = 1 + maybe 0 snd (find (\w -> fst w == nextWord) words)

-- getFreqnNext Map a word to his frequence and the next words with the probabilities
-- associated
getFreqnNext :: [String] -> Map.Map String (Int, [(String,Int)])
getFreqnNext [] = Map.empty
getFreqnNext [word] = Map.insertWith addValue word (1, [(".",1)]) Map.empty
getFreqnNext (w:ws) = Map.insertWith addValue w (1, [(head ws,1)]) $ getFreqnNext ws

-- Function to get the following words in sorted order
getNextsSorted :: Map.Map String (Int, [(String, Int)]) -> Map.Map String (Int, [String])
getNextsSorted = Map.map $ second (map fst . sortOn snd)

getStatsFromFile :: IO (Map.Map String (Int, [String]))
getStatsFromFile = getNextsSorted.getFreqnNext <$> getFiles

mapToStr :: Map.Map String (Int, [String]) -> String
mapToStr = Map.foldrWithKey (\key value str -> translateWord key value ++ str) "" 
  where translateWord word (freq, nextWords) = "{\"word\":"++show word
                                                  ++",\"freq\":"++show freq
                                                  ++",\"follows\":"++show (take 3 nextWords)++"}"

dictToStats :: IO ()
dictToStats = do
  stats <- getStatsFromFile
  writeFile "Statistics/result.txt" $ mapToStr stats