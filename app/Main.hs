{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
-- TODO : Ajouter poids (fréquences) dans l'arbre
import System.IO
     ( hClose, hGetContents, openFile, IOMode(ReadMode) )
import Data.List ( nub, intersperse )
import Data.Vector ( Vector, fromList, toList, imap, (!?), (!) )
import qualified Data.Vector as V ( map, find )
import Data.Aeson ( FromJSON, parseJSON, withObject, (.:), decodeStrict )
import Data.ByteString.Char8 ( pack )
import WordsTrees
--import Control.Parallel ( par, pseq )

type Keyboard = Vector Char

instance FromJSON CountedWords where
    parseJSON = withObject "CountedWords" $ \v -> CountedWords
        <$> v .: "word"
        <*> v .: "freq"

main :: IO ()
main = do
    file <- openFile "Py_frequence_2mots//freq2.txt" ReadMode
    contents <- hGetContents file
    let inputFreq = parseInput contents
    --print $ take 10 (map ((\(Just x) -> x).decodeStrict.pack) inputFreq :: [CountedWords])
    let dictionaryTree = listToTree (Node 0 []) $ map (fromMaybe.decodeStrict.pack) inputFreq
    --print $ V.find (\z -> 'a' == fst z) actualKeyboard
    putStrLn "Appuyez sur retour chariot pour corriger un mot ou sur tab + retour chariot pour compléter le mot en cours."
    putStrLn "Entrez un mot:"
    prompt dictionaryTree
    hClose file
      where prompt tree =
              do putStr "> "
                 line <- getLine
                 if null line
                   then return ()
                   else do print $ if last line == '\t' then completeWord tree $ init line
                                   else correctWord tree line
                           prompt tree
                           --print $ isReal t line

fromMaybe :: Maybe CountedWords -> CountedWords
fromMaybe Nothing = CountedWords "" 0
fromMaybe (Just s) = s

completeWord :: Tree Char -> String -> [String]
completeWord tree prefixe = take 10 . map (\(CountedWords s _) -> s) $ sortCountedWords $ giveSuffixe tree prefixe 

correctWord :: Tree Char -> String -> [String]
correctWord tree word = take 10 . map (\(CountedWords s _,_) -> s) . sortFreq .
                        quickSort . map (\x -> (x, strDiff (CountedWords word 0) x)) $
                        nub $ similarWords tree 2 word

correctLine :: Tree Char -> String -> String
correctLine tree line = assemble correctWords
    where correctWords = map (head . correctWord tree) $ words line --parseInput line
          assemble = tail . foldl (\w1 w2 -> w1 ++ " " ++ w2 ) ""

parseInput :: String -> [String]
parseInput [] = [[]]
parseInput (' ':q) = parseInput q
parseInput (t:' ':q) = [t] : parseInput q 
parseInput (t:q) = (t:head inputParsed):tail inputParsed
    where inputParsed = parseInput q

prettyPrint :: [String] -> IO ()
prettyPrint l = mapM_ putStr $ ["["] ++ intersperse ", " l ++ ["]\n"]

keyboardEn :: Keyboard
keyboardEn = fromList 
        [ 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p'
        , 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';'
        , 'z', 'x', 'c', 'v', 'b', 'n', 'm',',','.']

-- Défini la zone de proximité, pb : ind-1 pour a met p
nearIndices :: Integral a => a -> [a]
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

-- Composition pour rendre nearChars plus agréable : création des voisins
nearChars' :: Keyboard -> Vector [Char]
nearChars' = V.map (map (\(Just x) -> x)) . clearNearChars . charsPerimeter

-- Association entre caractère et ses voisins
associateNearChars :: Keyboard -> Vector [Char] -> Vector (Char, [Char])
associateNearChars keyboard perimeter = imap (\ind char -> (char, perimeter!ind)) keyboard 

-- Donne les voisins de chaque caractère
nearChars :: Keyboard -> Vector (Char, [Char])
nearChars keyboard = associateNearChars keyboard $ nearChars' keyboard

-- Clavier utilisé
actualKeyboard :: Vector (Char, [Char])
actualKeyboard = nearChars keyboardEn

outMaybeAssocList :: Maybe (a, [b]) -> [b]
outMaybeAssocList Nothing = []
outMaybeAssocList (Just(_,l)) = l

-- Calcul la distance de deux mots (dist de Hamming modifiée)
strDiff :: CountedWords -> CountedWords -> Int
strDiff (CountedWords x _) (CountedWords [] _) = length x
strDiff (CountedWords [] _) (CountedWords y _) = length y
strDiff wx@(CountedWords (x:xs) freqx) wy@(CountedWords (y:ys) freqy) =
    if x==y then strDiff (CountedWords xs freqx) (CountedWords ys freqy)
    else 2 + diffMin
        where diffMin = min diffMinq $ min (strDiff (CountedWords xs freqx) wy) (strDiff wx (CountedWords ys freqy))
              -- traiter le cas où ils sont "proches" :
              diffMinq = strDiff (CountedWords xs freqx) (CountedWords ys freqy)
                      - if elem x $ nearChar y then 1
                        else 0
              nearChar c = outMaybeAssocList $ V.find (\z -> c == fst z) actualKeyboard

quickSort :: [(CountedWords, Int)] -> [(CountedWords, Int)]
quickSort [] = []
quickSort (x:xs) = quickSort [w | w <- xs, snd w < snd x]
                ++ [x]
                ++ quickSort [w | w <- xs, snd w >= snd x]

freqFromCountedWords :: CountedWords -> Int
freqFromCountedWords (CountedWords _ f) = f

sortFreq :: [(CountedWords, Int)] -> [(CountedWords,Int)]
sortFreq [] = []
sortFreq (x@(CountedWords s fs, nearest):xs) =
       [w | w <- xs, snd w == nearest && fs < freqFromCountedWords (fst w)]
    ++ [x]
    ++ [w | w <- xs, snd w == nearest && fs >= freqFromCountedWords (fst w)]
    ++ sortFreq [w | w <- xs, snd w /= nearest]

sortCountedWords :: [CountedWords] -> [CountedWords]
sortCountedWords [] = []
sortCountedWords (cw:cws) = sortCountedWords [w | w <- cws, freqFromCountedWords cw < freqFromCountedWords w]
                        ++ [cw]
                        ++ sortCountedWords [w | w <- cws, freqFromCountedWords cw >= freqFromCountedWords w]