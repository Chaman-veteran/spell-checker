module Data.WordTree where

import Data.Function
import Data.List (nub)
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M (empty, foldrWithKey, insert)
import Data.Maybe (fromMaybe)
import Data.Aeson (FromJSON(parseJSON), (.:), parseJSON2)

data WordProperties = WordProperties {frequency :: Int, unknown :: [String]} deriving (Eq)

instance FromJSON WordProperties where
  parseJSON v = do
    (x, y) <- parseJSON v
    return $ WordProperties x y

data Tree a = Node {freqNFollowing :: WordProperties, branches :: Map a (Tree a)}

data CountedWords = CountedWords
  { word :: String,
    freqNNextWord :: WordProperties
  }
  deriving (Eq)

instance Ord CountedWords where
  compare = compare `on` frequency . freqNNextWord

instance Show CountedWords where
  show = word

-- Predicate to know if the word exists in the tree
isReal :: Ord a => [a] -> Tree a -> Bool
isReal [] (Node prop _) = frequency prop /= 0
isReal (w : ws) actualNode = maybe False (isReal ws) $ branches actualNode !? w

-- Return the frequence and the next probable words
-- of the word if he exists, Nothing else
freqNFollowingOf :: Ord a => [a] -> Tree a -> WordProperties
freqNFollowingOf [] (Node (WordProperties freq nextWords) _) = WordProperties freq nextWords
freqNFollowingOf (w : ws) actualNode = maybe (WordProperties 0 []) (freqNFollowingOf ws) $ branches actualNode !? w

-- Insert a word in a tree
inser :: CountedWords -> Tree Char -> Tree Char
inser (CountedWords [] (WordProperties freq nextWords)) (Node _ branches) = Node (WordProperties freq nextWords) branches
inser (CountedWords (w : ws) (WordProperties f nextWords)) (Node (WordProperties freq followingActual) branches) =
  Node (WordProperties freq followingActual) $ M.insert w (inser wordToInsert $ fromMaybe (Node (WordProperties 0 []) M.empty) next) branches
  where
    next = branches !? w
    wordToInsert = CountedWords ws (WordProperties f nextWords)

-- Insert a list of words in a tree
listToTree :: [CountedWords] -> Tree Char
listToTree = foldr inser (Node (WordProperties 0 []) M.empty)

-- Gives similar words (distance fixed by the snd parameter)
-- similarWord :: Tree -> distance max -> actual prefixe -> typed word -> [neighbour words]
similarWord :: Tree Char -> Int -> String -> String -> [CountedWords]
similarWord (Node (WordProperties freq nextWords) _) _ prefixe [] = [CountedWords prefixe (WordProperties freq nextWords) | freq /= 0]
similarWord actualNode 0 prefixe word = [CountedWords (prefixe ++ word) freqWord | frequency freqWord /= 0]
  where
    freqWord = freqNFollowingOf word actualNode
similarWord actualNode@(Node (WordProperties freq nextWords) branches) n prefixe word@(w : ws) =
  M.foldrWithKey searchWords [] branches
  where
    searchWords l nextTree accum =
      ( if l /= w
          then similarWord actualNode (n - 1) prefixe (l : ws)
          else similarWord nextTree n (prefixe ++ [w]) ws
      )
        ++ accum

similarWords :: Tree Char -> Int -> String -> [CountedWords]
similarWords tree distance word = nub $ similarWord tree distance [] word

-- Gives the tree associated to a prefix (i.e. the tree of possible suffixes)
possibleSuffixes :: String -> Tree Char -> Tree Char
possibleSuffixes [] tree = tree
possibleSuffixes (w : ws) tree = maybe (Node (WordProperties 0 []) M.empty) (possibleSuffixes ws) $ branches tree !? w

nextPossibilities :: Tree Char -> String -> [CountedWords]
nextPossibilities (Node freq branches) prefixe =
  M.foldrWithKey getPossibilities [CountedWords prefixe freq] branches
  where
    getPossibilities char subTree accum = nextPossibilities subTree (prefixe ++ [char]) ++ accum

giveSuffixe :: Tree Char -> String -> [CountedWords]
giveSuffixe tree prefixe = nextPossibilities (possibleSuffixes prefixe tree) prefixe
