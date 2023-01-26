module Helpers(boolToChar, charToBool, split, splitByFunc, removeWSpace, removeAllNonNumbersAndReadAsInt, countElem,
findBetween, findBetweenAndReturnRest, isInt, charToInt, replace, wordsWhen, splitBySublist) where

import Data.Char (isDigit)
import Data.List (isPrefixOf)

-- Split by a predicate
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'


boolToChar :: Bool -> Char
boolToChar True  = '1'
boolToChar False = '0'

charToBool :: Char -> Bool
charToBool '1' = True
charToBool '0' = False
charToBool c   = error ("charToBool: Invalid character" ++ [c])


splitBySublist :: Eq a => [a] -> [a] -> ([a], [a])
splitBySublist [] _ = ([], [])
splitBySublist _ [] = ([], [])
splitBySublist xs ys | ys `isPrefixOf` xs = ([], drop (length ys) xs)
                     | otherwise          = (x:ys', zs)
                     where (ys', zs) = splitBySublist (tail xs) ys
                           x         = head xs

split :: Eq a => [a] -> a -> ([a], [a])
split [] c = ([c], [])
split (x:xs) c | x == c    = ([], xs)
               | otherwise = (x:ys, zs)
               where (ys, zs) = split xs c
               
splitByFunc :: (a -> Bool) -> [a] -> ([a], [a])
splitByFunc _ [] = ([], [])
splitByFunc f (x:xs) | f x       = ([], xs)
                     | otherwise = (x:ys, zs)
                     where (ys, zs) = splitByFunc f xs             
                     
                     
removeWSpace :: String -> String
removeWSpace [] = []
removeWSpace (x:xs) | x == ' '  = removeWSpace xs
                    | otherwise = x:removeWSpace xs
                    
                    
removeAllNonNumbersAndReadAsInt :: String -> Int
removeAllNonNumbersAndReadAsInt s = read $ filter isDigit s

isInt :: String -> Bool
isInt x | null x        = False
        | all isDigit x = True
        | otherwise     = False

countElem :: Eq a => a -> [a] -> Int
countElem x = length . filter (==x)


findBetween :: Eq a => a -> a -> [a] -> [a]
findBetween _ _ [] = []
findBetween a b xs = takeWhile(/= b) $ drop 1 $ dropWhile(/=a) xs

findBetweenAndReturnRest :: Eq a => a -> a -> [a] -> ([a], [a])
findBetweenAndReturnRest _ _ [] = ([], [])
findBetweenAndReturnRest a b xs = (takeWhile(/= b) $ drop 1 $ dropWhile(/=a) xs, drop 1 $ dropWhile(/= b) xs)


charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9
charToInt c   = error ("charToInt: Invalid character" ++ [c])


replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace a b (x:xs) | x == a    = b:replace a b xs
                   | otherwise = x:replace a b xs