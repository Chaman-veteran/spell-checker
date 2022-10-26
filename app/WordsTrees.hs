module WordsTrees where
-- Passer avec Noeud + Branches et remplacer le booléen du Noeud
-- par un Maybe int : la proba du mot pour la prendre légèrement en compte
-- dans le calcul de distance
-- Ajouter prédiction du prochain mot
import System.IO
     ( hClose, hGetContents, openFile, IOMode(ReadMode) )
import Data.List ( nub, intersperse )
import Data.Bifunctor ( second )
--import Control.Parallel ( par, pseq )

data Tree a = Node Int [Branch a] 
data Branch a = B a (Tree a)


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

-- Insert dans un arbre un mot
inser :: Eq a => Tree a -> [a] -> Tree a
inser (Node freq branches) [] = Node 1 branches
inser (Node freq branches) (w:ws) =
        Node freq $ if null next then B w (inser (Node 0 []) ws) : branches
                    else B w (inser (branchSnd $ head next) ws) : follow
        where
            (next, follow) = separate branches w

-- Insert une liste de mots dans un arbre
listToTree :: Eq a => Tree a -> [[a]] -> Tree a
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
