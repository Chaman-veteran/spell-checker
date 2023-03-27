module StatsFromTxt where
import System.IO ( readFile, writeFile )
import System.Directory ( listDirectory )
import qualified Data.Map.Strict as Map ( Map, insert, empty, insertWith, foldrWithKey )
import Control.Monad ( forM )

-- TODO : Serialize the Map (or the Tree?) instead of the binary

getWords :: FilePath -> IO [String]
getWords file = words <$> readFile ("Dictionnaries//"++file)

getFiles :: IO [String]
getFiles = do
  files <- listDirectory "Dictionnaries"
  wordsPerFile <- forM files getWords
  return $ concat wordsPerFile

addValue :: (Int, [String]) -> (Int, [String]) -> (Int, [String])
addValue (freq, words) (incFreq, nextWord) = (freq+incFreq, nextWord++words)

-- TODO : Ordering the list of words following by most present
getFreqnNext :: [String] -> Map.Map String (Int, [String])
getFreqnNext [] = Map.empty
getFreqnNext [word] = Map.insertWith addValue word (1,["."]) Map.empty
getFreqnNext (w:ws) = Map.insertWith addValue w (1,[head ws]) $ getFreqnNext ws

getStatsFromFile :: IO (Map.Map String (Int, [String]))
getStatsFromFile = getFreqnNext <$> getFiles

mapToStr :: Map.Map String (Int, [String]) -> String
mapToStr = Map.foldrWithKey (\key value str -> translateWord key value ++ str) "" 
  where translateWord word (freq, nextWords) = "{\"word\":"++show word
                                                  ++",\"freq\":"++show freq
                                                  ++",\"follows\":"++show (take 3 nextWords)++"}"

statsToFile :: IO (Map.Map String (Int, [String])) -> IO ()
statsToFile stats = do
  toWrite <- mapToStr <$> stats
  writeFile "Statistics/result.txt" toWrite

dictToStats :: IO ()
dictToStats = statsToFile getStatsFromFile