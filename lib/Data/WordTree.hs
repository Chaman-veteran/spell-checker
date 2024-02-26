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
import Data.Aeson (FromJSON(parseJSON))

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
data CountedWord = CountedWord
  { word :: String
  , freqNInfo :: WordProperties
  }
  deriving (Eq)

instance Ord CountedWord where
  compare = compare `on` frequency . freqNInfo

instance Show CountedWord where
  show = word

-- | We represent non-existent words with nullProperties 
nullProperties :: WordProperties
nullProperties = WordProperties 0 []

-- | We represent the tree associated to the void dictionary with nullTree 
nullTree :: Tree a
nullTree = Node nullProperties M.empty

traverseTree :: Ord k => k -> Maybe (Tree k) -> Maybe (Tree k)
traverseTree c Nothing = Nothing
traverseTree c (Just actualNode) = branches actualNode !? c

-- | Predicate to know if a word exist in the tree
exists :: Tree Char -> String -> Bool
exists tree t = Just True == (True <$ foldr traverseTree (Just tree) t)

-- | Return the properties of a given word if it exists, the null word otherwise
propertiesOf :: String -> Tree Char -> WordProperties
propertiesOf t tree = maybe nullProperties properties (foldr traverseTree (Just tree) t)

-- | Insertion of a word in a tree
insert :: CountedWord -> Tree Char -> Tree Char
insert wordEnd@(CountedWord [] _) (Node _ branches) = Node (freqNInfo wordEnd) branches
insert (CountedWord (w : ws) (WordProperties f nextWords)) (Node propertiesExistingWord branches) =
  Node propertiesExistingWord $ M.insert w (insert wordToInsert $ fromMaybe nullTree next) branches
  where
    next = branches !? w
    wordToInsert = CountedWord ws (WordProperties f nextWords)

-- | Insertion of  a list of words in a tree
fromList :: [CountedWord] -> Tree Char
fromList = foldr insert nullTree

-- | Transform a map of CountedWords in a Tree
fromMap :: Map String (Int, [String]) -> Tree Char
fromMap = M.foldrWithKey (\key (f, i) tree -> insert (CountedWord key (WordProperties f i)) tree) nullTree

-- | Gives similar words from a suffixe as CountedWord (distance fixed by the snd parameter)
-- similarWord :: Tree -> max distance -> actual prefixe -> typed word -> [neighbour words]
similarWord :: Tree Char -> Int -> String -> String -> [CountedWord]
similarWord (Node propertiesWord _) _ prefixe [] = [CountedWord prefixe propertiesWord | frequency propertiesWord /= 0]
similarWord actualNode 0 prefixe word = [CountedWord (prefixe ++ word) freqWord | frequency freqWord /= 0]
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

-- | Gives similar words of a given one as CountedWord
similarWords :: Tree Char -> Int -> String -> [CountedWord]
similarWords tree distance word = nub $ similarWord tree distance [] word

-- | Gives the tree associated to a prefix (i.e. the tree of possible suffixes)
possibleSuffixes :: String -> Tree Char -> Tree Char
possibleSuffixes [] tree = tree
possibleSuffixes (w : ws) tree = maybe nullTree (possibleSuffixes ws) $ branches tree !? w

-- | Gives all words made out of possible suffixes obtained by visiting the actual subtree 
nextPossibilities :: Tree Char -> String -> [CountedWord]
nextPossibilities (Node freq branches) prefixe =
  M.foldrWithKey getPossibilities [CountedWord prefixe freq] branches
  where
    getPossibilities char subTree accum = filter existingCountedWord (nextPossibilities subTree (prefixe ++ [char])) 
                                            ++ accum
    -- ^ Gets all suffixes of the current prefixe (with the last letter being char) if we visit the sub tree subTree
    existingCountedWord = (/= 0) . (frequency.freqNInfo)

-- | Gives all possible suffixes to complete a word
giveSuffixe :: Tree Char -> String -> [CountedWord]
giveSuffixe tree prefixe = nextPossibilities (possibleSuffixes prefixe tree) prefixe
