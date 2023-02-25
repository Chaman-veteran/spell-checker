module WordsTrees where
-- TODO : Ajouter poids (fréquences) dans l'arbre
import Data.Bifunctor ( second )

data Tree a = Node Int [Branch a] 
data Branch a = B a (Tree a)

data CountedWords = CountedWords {
      word :: String
    , freq :: Int
    } deriving Show

branchFst :: Branch a -> a
branchFst (B c _) = c

branchSnd :: Branch a -> Tree a
branchSnd (B _ t) = t

-- Arbre (noeud) -> lettre -> noeud associé à la lettre
searchLetter :: Eq a => Tree a -> a -> [Tree a]
searchLetter (Node _ branches) letter = take 1 $ map branchSnd
                                        $ filter ((==) letter . branchFst) branches 

-- Dit si le mot existe dans l'arbre
isReal :: Eq a => Tree a -> [a] -> Bool
isReal (Node freq _) [] = freq /= 0
isReal actualNode (w:ws) = not (null next) && isReal hnext ws
    where
        next = searchLetter actualNode w
        hnext = head next

-- Sépare une liste de branche : la branche (si elle existe) avec la lettre et les autres
separate :: Eq a => [Branch a] -> a -> ([Branch a], [Branch a])
separate [] _ = ([], [])
separate ((B c tree):tbranch) letter =
    if c==letter then ([B c tree], tbranch)
    else second (B c tree :) $ separate tbranch letter

-- Insert un mot dans un arbre
inser :: Tree Char -> CountedWords -> Tree Char
inser (Node _ branches) (CountedWords [] freq) = Node freq branches
inser (Node freq branches) (CountedWords (w:ws) f) =
        Node freq $ if null next then B w (inser (Node 0 []) toInsert) : branches
                    else B w (inser (branchSnd $ head next) toInsert) : follow
        where
            (next, follow) = separate branches w
            toInsert = CountedWords ws f

-- Insert une liste de mots dans un arbre
listToTree :: Tree Char -> [CountedWords] -> Tree Char
listToTree = foldl inser

-- Donne la liste des lettres possibles pour continuer dans l'arbre
fromNodesToLetters :: Foldable t => t (Branch a) -> [a]
fromNodesToLetters = foldr (\(B char _) -> (:) char) []

-- Donne les mots similaires à un mot donné (distance fixée par le 2è paramètre)
similarWord :: (Eq p, Num p, Eq a) => Tree a -> p -> [a] -> [a] -> [[a]]
-- similarWord :: Arbre dico -> profondeur -> préfixe actuel -> mot tapé -> [mots proches]
similarWord (Node freq _) _ prefixe [] = [prefixe | freq /= 0]
similarWord actualNode 0 prefixe word = [if isReal actualNode word then prefixe++word else prefixe]
similarWord actualNode@(Node freq branches) n prefixe word@(w:ws) =
    let searchWords nextBranch@(B l nextTree) =
                        if l/=w then similarWord actualNode (n-1) prefixe (l:ws)
                        else similarWord nextTree n (prefixe++[w]) ws in
    concatMap searchWords branches

similarWords :: (Eq p, Num p, Eq a) => Tree a -> p -> [a] -> [[a]]
similarWords tree distance = similarWord tree distance []
