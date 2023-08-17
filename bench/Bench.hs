{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson (FromJSON, decodeStrict, parseJSON, withObject, (.:))
import Data.ByteString.Char8 (pack)
import Data.Maybe (mapMaybe)
import Control.DeepSeq (NFData(..))
import Criterion.Main (defaultMain, bench, bgroup, env, whnf) 

import Data.WordTree

instance FromJSON CountedWord where
  parseJSON = withObject "CountedWord" $ \v ->
    CountedWord
      <$> v .: "word"
      <*> v .: "properties"

instance NFData WordProperties where
    rnf (WordProperties f i) = rnf f `seq` rnf i

instance NFData a => NFData (Tree a) where
    rnf (Node p b) = rnf p `seq` rnf b

setupEnv :: IO (Tree Char)
setupEnv = do
  contents <- readFile "Statistics//result.txt"
  let inputFreq = words contents
  let dictionaryTree = listToTree $ mapMaybe (decodeStrict . pack) inputFreq
  return dictionaryTree

main :: IO ()
main = defaultMain [
  env setupEnv $ \dictionaryTree -> bgroup "Completion" [
    bench "Completion" $ whnf (`giveSuffixe` "") dictionaryTree
  ],
  env setupEnv $ \dictionaryTree -> bgroup "Correction" [
    bench "Correction" $ whnf (\d -> similarWords d 4 "Haskell") dictionaryTree 
  ]
  ]
