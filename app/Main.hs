{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module Main where
-- Passer avec Noeud + Branches et remplacer le booléen du Noeud
-- par un Maybe int : la proba du mot pour la prendre légèrement en compte
-- dans le calcul de distance
-- Ajouter prédiction du prochain mot
import System.IO
     ( hClose, hGetContents, openFile, IOMode(ReadMode) )
import Data.List ( nub, intersperse )
import Data.Vector ( Vector, fromList, toList, imap, (!?), (!) )
import qualified Data.Vector as V ( map, find )
import Data.Bifunctor ( second )
--import Control.Parallel ( par, pseq )

data Tree a = Node Bool a [Tree a] deriving (Show, Eq) 
type Keyboard = Vector Char

main :: IO ()
main = do
    file <- openFile "en_GB.dic" ReadMode
    contents <- hGetContents file
    let dictionaryTree = listToTree (Node False '0' []) $ words contents
    --print $ V.find (\z -> 'a' == fst z) actualKeyboard
    putStrLn "Entrez un mot:"
    prompt dictionaryTree
    hClose file
      where prompt tree =
              do putStr "> "
                 line <- getLine
                 if null line
                   then return ()
                   else do print $ correctLine tree line
                           prompt tree
                           --print $ isReal t line

correctWord :: Tree Char -> String -> [String]
correctWord tree word = take 10 . map fst . quickSort.map (\x -> (x, strDiff word x)) $
                        nub $ similarWords tree 2 word

correctLine :: Tree Char -> String -> String
correctLine tree line = assemble correctWords
    where correctWords = map (head . correctWord tree) $ parseInput line
          assemble = tail . foldl (\w1 w2 -> w1 ++ " " ++ w2 ) ""

parseInput :: String -> [String]
parseInput [] = [[]]
parseInput (' ':q) = parseInput q
parseInput (t:' ':q) = [t] : parseInput q 
parseInput (t:q) = (t:head inputParsed):tail inputParsed
    where inputParsed = parseInput q

prettyPrint :: [String] -> IO ()
prettyPrint l = mapM_ putStr $ ["["] ++ intersperse ", " l ++ ["]\n"]

-- Arbre (noeud) -> lettre -> noeud associé à la lettre ? (bool)
searchLetter :: Eq a => Tree a -> a -> Bool
searchLetter (Node _ letter _) = (==) letter

-- Dit si le mot existe dans l'arbre
isReal :: Tree Char -> [Char] -> Bool
isReal (Node is_word _ _) "" = is_word
isReal (Node is_word letter list_trees) (w:ws) = not (null next) && isReal hnext ws
    where
        next = take 1 $ filter (`searchLetter` w) list_trees
        hnext = head next

-- Sépare une liste d'arbre : l'arbre contenant la lettre en racine et les autres
separate :: Eq t => [Tree t] -> t -> ([Tree t], [Tree t])
separate [] _ = ([], [])
separate (hd:tl) letter =
    if searchLetter hd letter then ([hd], tl)
    else second (hd :) $ separate tl letter

-- Insert dans un arbre un mot
inser :: Tree Char -> [Char] -> Tree Char
inser (Node _ c list_trees) "" = Node True c list_trees
inser (Node is_word c list_trees) (w:ws) =
        Node is_word c $ if null next then inser (Node False w []) ws : list_trees
                         else inser (head next) ws : follow
        where
            separateTrees = separate list_trees w
            next = fst separateTrees
            follow = snd separateTrees

-- Insert une liste de mots dans un arbre
listToTree :: Tree Char -> [[Char]] -> Tree Char
listToTree = foldl inser

-- Donne la liste des lettres possibles pour continuer dans l'arbre
fromNodesToLetters :: Foldable t => t (Tree Char) -> [Char]
fromNodesToLetters = foldr (\(Node _ l _) -> (:) l) ""

-- Donne les mots similaires à un mot donné (distance fixée par le 2è paramètre)
similarWord :: (Eq p, Num p) => Tree Char -> p -> [Char] -> [Char] -> [[Char]]
similarWord (Node True _ _) _ prefixe "" = [prefixe]
similarWord _ _ _ "" = []
similarWord actualNode 0 prefixe word = [if isReal actualNode word then prefixe++word else prefixe]
similarWord actualNode@(Node is_word c list_trees) n prefixe word@(w:ws) =
    let searchWords nextTree@(Node b l list_ts) =
                        if l/=w then similarWord actualNode (n-1) prefixe (l:ws)
                        else similarWord nextTree n (prefixe++[w]) ws in
    concatMap searchWords list_trees

similarWords :: (Eq p, Num p) => Tree Char -> p -> [Char] -> [[Char]]
similarWords tree distance = similarWord tree distance ""

keyboardEn :: Keyboard
keyboardEn = fromList 
        [ 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p'
        , 'a', 's', 'd', 'f', 'g', 'h', 'i', 'j', 'k', 'l'
        , 'z', 'x', 'c', 'v', 'b', 'n', 'm']

-- Défini la zone de proximité, pb : ind-1 pour a met p
nearIndices ind = case  ind `mod` 10 of
    0 -> [ind+1, ind+10, ind-10, ind-9, ind+11]
    9 -> [ind-1, ind+10, ind-10, ind+9, ind-11]
    _ -> [ind-1, ind+1, ind+10, ind-10, ind-11, ind+11, ind-9, ind+9]

-- Donne les charactères proches pour chaque charactère du clavier
charsPerimeter :: Keyboard -> Vector [Maybe Char]
charsPerimeter keyboard = imap (\ind _ -> map (keyboard!?) $ nearIndices ind) keyboard

-- Enlève les Nothing pour les problèmes aux extremités
clearNearChars :: Vector [Maybe Char] -> Vector [Maybe Char]
clearNearChars = V.map $ filter (/= Nothing)

-- Composition pour rendre nearChars plus agréable
nearChars' :: Keyboard -> Vector [Char]
nearChars' = V.map (map (\(Just x) -> x)) . clearNearChars . charsPerimeter

associateNearChars :: Keyboard -> Vector [Char] -> Vector (Char, [Char])
associateNearChars keyboard perimeter = imap (\ind char -> (char, perimeter!ind)) keyboard 

nearChars :: Keyboard -> Vector (Char, [Char])
nearChars keyboard = associateNearChars keyboard $ nearChars' keyboard

actualKeyboard :: Vector (Char, [Char])
actualKeyboard = nearChars keyboardEn

outMaybeAssocList :: Maybe (a, [b]) -> [b]
outMaybeAssocList Nothing = []
outMaybeAssocList (Just(_,l)) = l

-- Calcul la distance de deux mots (dist de Hamming modifiée)
strDiff :: [Char] -> [Char] -> Int
strDiff x "" = length x
strDiff "" y = length y
strDiff (x:xs) (y:ys) = 
    if x==y then strDiff xs ys
    else  2 + diffMin
        where   diffMin = min diffMinq $ min (strDiff xs (y:ys)) (strDiff (x:xs) ys)
                -- traiter le cas où ils sont "proches" :
                diffMinq = strDiff xs ys
                        - if elem x $ nearChar y then 1
                          else 0
                nearChar c = outMaybeAssocList $ V.find (\z -> c == fst z) actualKeyboard
 
quickSort :: Ord a2 => [(a1, a2)] -> [(a1, a2)]
quickSort [] = []
quickSort (x:xs) = quickSort [w | w <- xs, snd w < snd x]
                ++ [x]
                ++ quickSort [w | w <- xs, snd w >= snd x]
