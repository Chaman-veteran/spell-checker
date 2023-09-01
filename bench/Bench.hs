{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson (FromJSON, decodeStrict, parseJSON, withObject, (.:))
import Data.ByteString.Char8 (pack)
import Data.Maybe (mapMaybe)
import Control.DeepSeq (NFData(..), force)
import Criterion.Main (defaultMain, bench, bgroup, env, nfIO, nf)
import Codec.Serialise (readFileDeserialise)

import Data.WordTree

instance FromJSON CountedWord where
  parseJSON = withObject "CountedWord" $ \v ->
    CountedWord
      <$> v .: "word"
      <*> v .: "properties"

instance NFData WordProperties where
  rnf (WordProperties frequency info) = rnf frequency `seq` rnf info

instance NFData CountedWord where
  rnf (CountedWord word properties) = rnf word `seq` rnf properties  

instance NFData a => NFData (Tree a) where
  rnf (Node properties branches) = rnf properties `seq` rnf branches

setupEnv :: IO (Tree Char)
setupEnv = getTreeFromMap

getTreeFromMap :: IO (Tree Char)
getTreeFromMap = do 
      inputFreq <- readFileDeserialise "SerializedStatistics/result" 
      return $ fromMap inputFreq

getTreeFromStr :: IO (Tree Char)
getTreeFromStr = do
  contents <- readFile "Statistics//result.txt"
  let inputFreq = words contents
  let dictionaryTree = fromList $ mapMaybe (decodeStrict . pack) inputFreq
  return dictionaryTree

main :: IO ()
main = defaultMain [
  bgroup "Tree" [
    bench "TreeFromStr" $ nfIO getTreeFromStr,
    bench "TreeFromMap" $ nfIO getTreeFromMap
  ],
  env setupEnv $ \dictionaryTree -> bgroup "Completion" [
    bench "Completion" $ nf (`giveSuffixe` "") dictionaryTree
  ],
  env setupEnv $ \dictionaryTree -> bgroup "Correction" [
    bench "Correction" $ nf (\d -> similarWords d 3 "Haskell") dictionaryTree 
  ]
  ]
