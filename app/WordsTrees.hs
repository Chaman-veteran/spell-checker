module WordsTrees where
import Data.Bifunctor ( second )
import Data.List ( nub )

data Tree a = Node Int [Branch a] 
data Branch a = B { character :: a, subTree :: Tree a }

data CountedWords = CountedWords {
      word :: String
    , freq :: Int
    } deriving (Eq)

instance Ord CountedWords where
    compare (CountedWords _ f) (CountedWords _ g) = compare g f

instance Show CountedWords where
    show (CountedWords w _) = w


-- Tree (node) -> letter -> node associed to the letter
searchLetter :: Eq a => Tree a -> a -> [Tree a]
searchLetter (Node _ branches) letter = take 1 $ map subTree
                                        $ filter ((==) letter . character) branches 

-- Predicate to know if the word exists in the tree
isReal :: Eq a => Tree a -> [a] -> Bool
isReal (Node freq _) [] = freq /= 0
isReal actualNode (w:ws) = not (null next) && isReal hnext ws
    where
        next = searchLetter actualNode w
        hnext = head next

-- Return the frequence of the word if he exists, 0 else
freqOf :: Eq a => Tree a -> [a] -> Int
freqOf (Node freq _) [] = freq
freqOf actualNode (w:ws) = if not (null next) then freqOf hnext ws else 0
    where
        next = searchLetter actualNode w
        hnext = head next

-- Separate a list of branchs : the one (if she exists) with the letter and others
separate :: Eq a => [Branch a] -> a -> ([Branch a], [Branch a])
separate [] _ = ([], [])
separate ((B c tree):tbranch) letter =
    if c==letter then ([B c tree], tbranch)
    else second (B c tree :) $ separate tbranch letter

-- Insert a word in a tree
inser :: Tree Char -> CountedWords -> Tree Char
inser (Node _ branches) (CountedWords [] freq) = Node freq branches
inser (Node freq branches) (CountedWords (w:ws) f) =
        Node freq $ if null next then B w (inser (Node 0 []) toInsert) : branches
                    else B w (inser (subTree $ head next) toInsert) : follow
        where
            (next, follow) = separate branches w
            toInsert = CountedWords ws f

-- Insert a list of words in a tree
listToTree :: Tree Char -> [CountedWords] -> Tree Char
listToTree = foldl inser

-- Gives the list of possible letters to continue in the tree
fromNodesToLetters :: Foldable t => t (Branch a) -> [a]
fromNodesToLetters = foldr (\(B char _) -> (:) char) []

-- Gives similar words (distance fixed by the snd parameter)
similarWord :: Tree Char -> Int -> String -> String -> [CountedWords]
-- similarWord :: Tree -> distance max -> actual prefixe -> typed word -> [neighbour words]
similarWord (Node freq _) _ prefixe [] = [CountedWords prefixe freq | freq /= 0]
similarWord actualNode 0 prefixe word = [CountedWords (prefixe++word) freqWord | freqWord /= 0]
    where freqWord = freqOf actualNode word
similarWord actualNode@(Node freq branches) n prefixe word@(w:ws) =
    let searchWords nextBranch@(B l nextTree) =
                        if l/=w then similarWord actualNode (n-1) prefixe (l:ws)
                        else similarWord nextTree n (prefixe++[w]) ws in
    concatMap searchWords branches

similarWords :: Tree Char -> Int -> String -> [CountedWords]
similarWords tree distance word = nub $ similarWord tree distance [] word

-- Gives the tree associated to a prefix (i.e. the tree of possible suffixes)
possibleSuffixes :: Tree Char -> String -> Tree Char
possibleSuffixes tree [] = tree 
possibleSuffixes actualNode (w:ws) = 
    if not (null next) then possibleSuffixes hnext ws else Node 0 []
    where
        next = searchLetter actualNode w
        hnext = head next


nextPossibilities :: Tree Char -> String -> [CountedWords]
nextPossibilities (Node freq []) prefixe = [CountedWords prefixe freq]
nextPossibilities (Node freq (b:bs)) prefixe = nextPossibilities (Node freq bs) prefixe
                                            ++ nextPossibilities (subTree b) (prefixe++[character b])

giveSuffixe :: Tree Char -> String -> [CountedWords]
giveSuffixe tree suffixe = nextPossibilities (possibleSuffixes tree suffixe) suffixe