{-# LANGUAGE InstanceSigs #-}

module WordsTrees where
import Data.List ( nub )
import Data.Maybe ( fromMaybe )
import qualified Data.Map.Strict as M ( insert, empty, foldrWithKey )
import Data.Map.Strict ( Map, (!?) )

type WordProperties = (Int, [String])

data Tree a = Node {freqNFollowing :: WordProperties , branches :: Map a (Tree a)}

data CountedWords = CountedWords {
      word :: String
    , freqNNextWord :: WordProperties
    } deriving (Eq)

instance Ord CountedWords where
    compare :: CountedWords -> CountedWords -> Ordering
    compare (CountedWords _ (f,_)) (CountedWords _ (g,_)) = compare g f

instance Show CountedWords where
    show :: CountedWords -> String
    show (CountedWords w _) = w

-- Predicate to know if the word exists in the tree
isReal :: Ord a => [a] -> Tree a -> Bool
isReal [] (Node (freq,_) _) = freq /= 0
isReal (w:ws) actualNode = maybe False (isReal ws) $ branches actualNode !? w

-- Return the frequence and the next probable words
-- of the word if he exists, Nothing else
freqNFollowingOf :: Ord a => [a] -> Tree a  -> (Int, [String])
freqNFollowingOf [] (Node (freq, nextWords) _) = (freq, nextWords)
freqNFollowingOf (w:ws) actualNode = maybe (0,[]) (freqNFollowingOf ws) $ branches actualNode !? w

-- Insert a word in a tree
inser :: CountedWords -> Tree Char -> Tree Char
inser (CountedWords [] (freq, nextWords)) (Node _ branches) = Node (freq, nextWords) branches
inser (CountedWords (w:ws) (f, nextWords)) (Node (freq, followingActual) branches) = 
    Node (freq, followingActual) $ M.insert w (inser wordToInsert $ fromMaybe (Node (0,[]) M.empty) next) branches
        where
            next = branches !? w
            wordToInsert = CountedWords ws (f, nextWords)

-- Insert a list of words in a tree
listToTree :: [CountedWords] -> Tree Char
listToTree = foldr inser (Node (0,[]) M.empty)

-- Gives similar words (distance fixed by the snd parameter)
-- similarWord :: Tree -> distance max -> actual prefixe -> typed word -> [neighbour words]
similarWord :: Tree Char -> Int -> String -> String -> [CountedWords]
similarWord (Node (freq, nextWords) _) _ prefixe [] = [CountedWords prefixe (freq, nextWords) | freq /= 0]
similarWord actualNode 0 prefixe word = [CountedWords (prefixe++word) freqWord | fst freqWord /= 0]
        where freqWord = freqNFollowingOf word actualNode
similarWord actualNode@(Node (freq, nextWords) branches) n prefixe word@(w:ws) =
    M.foldrWithKey searchWords [] branches
        where searchWords l nextTree accum =
                            (if l/=w then similarWord actualNode (n-1) prefixe (l:ws)
                             else similarWord nextTree n (prefixe++[w]) ws) ++ accum

similarWords :: Tree Char -> Int -> String -> [CountedWords]
similarWords tree distance word = nub $ similarWord tree distance [] word

-- Gives the tree associated to a prefix (i.e. the tree of possible suffixes)
possibleSuffixes :: String -> Tree Char -> Tree Char
possibleSuffixes [] tree = tree 
possibleSuffixes (w:ws) tree = maybe (Node (0,[]) M.empty) (possibleSuffixes ws) $ branches tree !? w


nextPossibilities :: Tree Char -> String -> [CountedWords]
nextPossibilities (Node freq branches) prefixe =
    M.foldrWithKey getPossibilities [CountedWords prefixe freq] branches
        where getPossibilities char subTree accum = nextPossibilities subTree (prefixe++[char]) ++ accum

giveSuffixe :: Tree Char -> String -> [CountedWords]
giveSuffixe tree prefixe = nextPossibilities (possibleSuffixes prefixe tree) prefixe
