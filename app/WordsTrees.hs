{-# LANGUAGE InstanceSigs #-}

module WordsTrees where
import Data.List ( nub )
import Data.Maybe ( fromMaybe )
import qualified Data.Map.Strict as Map ( Map, insert, empty, foldrWithKey )
import Data.Map.Strict ( (!?) )

data Tree a = Node {frequence :: Int, branches :: Map.Map a (Tree a)}

data CountedWords = CountedWords {
      word :: String
    , freq :: Int
    } deriving (Eq)

instance Ord CountedWords where
    compare :: CountedWords -> CountedWords -> Ordering
    compare (CountedWords _ f) (CountedWords _ g) = compare g f

instance Show CountedWords where
    show :: CountedWords -> String
    show (CountedWords w _) = w

-- Predicate to know if the word exists in the tree
isReal :: Ord a => [a] -> Tree a -> Bool
isReal [] (Node freq _) = freq /= 0
isReal (w:ws) actualNode = maybe False (isReal ws) $ branches actualNode !? w

-- Return the frequence of the word if he exists, Nothing else
freqOf :: Ord a => [a] -> Tree a  -> Int
freqOf [] (Node freq _) = freq
freqOf (w:ws) actualNode = maybe 0 (freqOf ws) $ branches actualNode !? w

-- Insert a word in a tree
inser :: CountedWords -> Tree Char -> Tree Char
inser (CountedWords [] freq) (Node _ branches) = Node freq branches
inser (CountedWords (w:ws) f) (Node freq branches) = 
    Node freq $ Map.insert w (inser wordToInsert $ fromMaybe (Node 0 Map.empty) next) branches
        where
            next = branches !? w
            wordToInsert = CountedWords ws f

-- Insert a list of words in a tree
listToTree :: [CountedWords] -> Tree Char
listToTree = foldr inser (Node 0 Map.empty)

-- Gives similar words (distance fixed by the snd parameter)
similarWord :: Tree Char -> Int -> String -> String -> [CountedWords]
-- similarWord :: Tree -> distance max -> actual prefixe -> typed word -> [neighbour words]
similarWord (Node freq _) _ prefixe [] = [CountedWords prefixe freq | freq /= 0]
similarWord actualNode 0 prefixe word = [CountedWords (prefixe++word) freqWord | freqWord /= 0]
        where freqWord = freqOf word actualNode
similarWord actualNode@(Node freq branches) n prefixe word@(w:ws) =
    Map.foldrWithKey searchWords [] branches
        where searchWords l nextTree accum =
                            (if l/=w then similarWord actualNode (n-1) prefixe (l:ws)
                             else similarWord nextTree n (prefixe++[w]) ws) ++ accum

similarWords :: Tree Char -> Int -> String -> [CountedWords]
similarWords tree distance word = nub $ similarWord tree distance [] word

-- Gives the tree associated to a prefix (i.e. the tree of possible suffixes)
possibleSuffixes :: String -> Tree Char -> Tree Char
possibleSuffixes [] tree = tree 
possibleSuffixes (w:ws) tree = maybe (Node 0 Map.empty) (possibleSuffixes ws) $ branches tree !? w


nextPossibilities :: Tree Char -> String -> [CountedWords]
nextPossibilities (Node freq branches) prefixe =
    Map.foldrWithKey getPossibilities [CountedWords prefixe freq] branches
        where getPossibilities char subTree accum = nextPossibilities subTree (prefixe++[char]) ++ accum

giveSuffixe :: Tree Char -> String -> [CountedWords]
giveSuffixe tree prefixe = nextPossibilities (possibleSuffixes prefixe tree) prefixe