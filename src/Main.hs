{-# LANGUAGE DeriveFunctor #-}
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

import System.IO (IOMode (ReadMode), hSetBuffering,
    hFlush, stdout, stdin, BufferMode (NoBuffering))
import System.IO.NoBufferingWorkaround (initGetCharNoBuffering, getCharNoBuffering)
import System.Info (os)
import Codec.Serialise (readFileDeserialise)

import Data.WordTree (Tree, fromMap)
import SpellCheckerInterface (completeWord, correctWord)

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

