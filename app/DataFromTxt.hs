module DataFromTxt where
import System.IO ( readFile )
import System.Directory ( listDirectory )
import qualified Data.Map.Strict as Map ( Map, insert, empty, insertWith )
import Control.Monad ( forM, foldM )


getWords :: FilePath -> IO [String]
getWords file = words <$> readFile file

getFiles :: IO [String]
getFiles = do
  files <- listDirectory "Dictionnaries"
  wordsPerFile <- forM files getWords
  return $ concat wordsPerFile

getFreq :: [String] -> Map.Map String Int
getFreq = foldr (\x -> Map.insertWith (+) x 1) Map.empty
