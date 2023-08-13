-- --------------------------------------------------------------------------
-- |
-- Module      :  Data.WordTree
--
--
-- A module for generic functions on trees made out of words.
--
-----------------------------------------------------------------------------

module Data.WordTree where

import Data.Function (on)
import Data.List (nub)
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M (empty, foldrWithKey, insert)
import Data.Maybe (fromMaybe)
import Data.Aeson (FromJSON(parseJSON), (.:), parseJSON2)

-- | Data linked to a word
data WordProperties = WordProperties {frequency :: Int, info :: [String]} deriving (Eq)

instance FromJSON WordProperties where
  parseJSON v = do
    (x, y) <- parseJSON v
    return $ WordProperties x y

-- | Words are spelled out on each branches of the tree where
-- nodes represent ends of (maybe non-existent) words
data Tree a = Node { properties :: WordProperties -- ^ Properties (of nul frequency if the word doesn't exists)
                   , branches :: Map a (Tree a) -- ^ To each valid following leter is associated a new tree
                   }
-- | The tree needs words along with their frequency and
-- informations. We also need for future process.
-- The trees take CountedWords as inputs and we give CountedWods
-- as output when giving similar words
data CountedWords = CountedWords
  { word :: String
  , freqNInfo :: WordProperties
  }
  deriving (Eq)

instance Ord CountedWords where
  compare = compare `on` frequency . freqNInfo

instance Show CountedWords where
  show = word

-- | We represent non-existent words as the nullWord 
nullWord :: WordProperties
nullWord = WordProperties 0 []

-- | Predicate to know if a word exist in the tree
isReal :: Ord a => [a] -> Tree a -> Bool
isReal [] (Node propertiesWord _) = frequency propertiesWord /= 0
isReal (w : ws) actualNode = maybe False (isReal ws) $ branches actualNode !? w

-- | Return the properties of a given word if it exists, the null word otherwise
propertiesOf :: Ord a => [a] -> Tree a -> WordProperties
propertiesOf [] (Node propertiesWord _) = propertiesWord
propertiesOf (w : ws) actualNode = maybe nullWord (propertiesOf ws) $ branches actualNode !? w

-- | Insertion of a word in a tree
inser :: CountedWords -> Tree Char -> Tree Char
inser wordEnd@(CountedWords [] _) (Node _ branches) = Node (freqNInfo wordEnd) branches
inser (CountedWords (w : ws) (WordProperties f nextWords)) (Node propertiesExistingWord branches) =
  Node propertiesExistingWord $ M.insert w (inser wordToInsert $ fromMaybe (Node nullWord M.empty) next) branches
  where
    next = branches !? w
    wordToInsert = CountedWords ws (WordProperties f nextWords)

-- | Insertion of  a list of words in a tree
listToTree :: [CountedWords] -> Tree Char
listToTree = foldr inser (Node nullWord M.empty)

-- | Gives similar words from a suffixe as CountedWords (distance fixed by the snd parameter)
-- similarWord :: Tree -> max distance -> actual prefixe -> typed word -> [neighbour words]
similarWord :: Tree Char -> Int -> String -> String -> [CountedWords]
similarWord (Node propertiesWord _) _ prefixe [] = [CountedWords prefixe propertiesWord | frequency propertiesWord /= 0]
similarWord actualNode 0 prefixe word = [CountedWords (prefixe ++ word) freqWord | frequency freqWord /= 0]
  where
    freqWord = propertiesOf word actualNode
similarWord actualNode@(Node _ branches) n prefixe word@(w : ws) =
  M.foldrWithKey searchWords [] branches
  where
    searchWords l nextTree accum =
      ( if l /= w
          then similarWord actualNode (n - 1) prefixe (l : ws)
          else similarWord nextTree n (prefixe ++ [w]) ws
      )
        ++ accum

-- | Gives similar words of a given one as CountedWords
similarWords :: Tree Char -> Int -> String -> [CountedWords]
similarWords tree distance word = nub $ similarWord tree distance [] word

-- | Gives the tree associated to a prefix (i.e. the tree of possible suffixes)
possibleSuffixes :: String -> Tree Char -> Tree Char
possibleSuffixes [] tree = tree
possibleSuffixes (w : ws) tree = maybe (Node nullWord M.empty) (possibleSuffixes ws) $ branches tree !? w

-- | Gives all words made out of possible suffixes obtained by visiting the actual subtree 
nextPossibilities :: Tree Char -> String -> [CountedWords]
nextPossibilities (Node freq branches) prefixe =
  M.foldrWithKey getPossibilities [CountedWords prefixe freq] branches
  where
    getPossibilities char subTree accum = nextPossibilities subTree (prefixe ++ [char]) ++ accum

-- | Gives all possible suffixes to complete a word
giveSuffixe :: Tree Char -> String -> [CountedWords]
giveSuffixe tree prefixe = nextPossibilities (possibleSuffixes prefixe tree) prefixe
