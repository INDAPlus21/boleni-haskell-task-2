module F1 where

import Data.Char (isAlpha)

-- example with fib 3
-- fib 3 = infib 0 1 1 --> infib 1 1 2
-- 2: infib 1 1 2 --> infib 1 2 3
-- 3: infib 1 2 3 = b = 2
-- https://stackoverflow.com/questions/44874253/fibonacci-in-haskell
-- rewritten so that it is easier to understand
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = 
    let infib a b k
            | k == n    = b
            | otherwise = infib b (a+b) (k+1)
    in infib 0 1 1

-- Defining the consonants that are used 
-- in rovarsprak & karpsravor
consonants = "bcdfghjklmnpqrstvwxz"
isConsonant :: Char -> Bool
isConsonant x = x `elem` consonants

rovarsprak :: String -> String 
rovarsprak [] = []
rovarsprak [x] = if isConsonant x
    then [x, 'o', x]
    else [x]
rovarsprak (x:xs) = rovarsprak [x] ++ rovarsprak xs

-- Why this makes sense
-- Ex       "lololol"
-- Splits   "lololol" into 'l' : "ololol"
-- Assumes, because 'l' is a consonant,
-- that the following two chars are added from rovarsprak.
-- Thereafter removing first 2 chars 'ol' from "ololol"
-- Leaving "olol", then returing 'l' : karpsravor "olol"
-- o is not consonant, there4 returns 'o' : karpsravor "lol"
-- l is consonant, removes "ol" in accordance with above principle
-- returns 'l' : karpsravor ""
-- karpsravor "" = "" per base case definition
-- Final return is therefore 'lol', which is correct
karpsravor :: String -> String
karpsravor [] = []
karpsravor (x:xs) = if isConsonant x
    then x : karpsravor (drop 2 xs)
    else x : karpsravor xs

split :: String ->  [String]
split [] = []
split s = clean (inSplit [] "" s) where
    inSplit :: [String] -> String -> String -> [String]
    inSplit a cS ""     = cS:a
    inSplit a cS (x:xs)
        | isAlpha x     = inSplit a (cS ++ [x]) xs
        | otherwise     = inSplit (cS:a) "" xs
    clean [] = []
    clean a = [x | x <- a, not (null x)]

medellangd :: String -> Double
medellangd [] = 0
medellangd s = fromIntegral(sum [sum [1 | _ <- xs] | xs <- split s]) / fromIntegral(length (split s))


-- skyffla osv