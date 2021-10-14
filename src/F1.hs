module F1 where

import Data.Char (isAlpha)

-- example with fib 3
-- fib 3 = infib 0 1 1 --> infib 1 1 2
-- 2: infib 1 1 2 --> infib 1 2 3
-- 3: infib 1 2 3 = b = 2
-- https://stackoverflow.com/questions/44874253/fibonacci-in-haskell
-- rewritten so that it is easier to understand
fib  :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = 
    let infib a b k
            | k == n    = b
            | otherwise = infib b (a+b) (k+1)
    in infib 0 1 1

-- Defining the consonants that are used 
-- in rovarsprak & karpsravor
isConsonant    :: Char -> Bool
isConsonant x   = x `elem` consonants where
    consonants  = "bcdfghjklmnpqrstvwxz"

rovarsprak   :: String -> String 
rovarsprak [] = []
rovarsprak [x] 
    | isConsonant x = [x, 'o', x]
    | otherwise     = [x]
rovarsprak (x:xs) = rovarsprak [x] ++ rovarsprak xs  -- I was worried that take unneccesary computing
-- but since the tail xs is being added to rovarsprak [x] = [x, 'o', x] if x is cons then it should max require 3 times the amount of letters
-- of the given word in operations

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
karpsravor         :: String -> String
karpsravor []       = []
karpsravor (x:xs)   = if isConsonant x
    then x : karpsravor (drop 2 xs)
    else x : karpsravor xs

split      :: String ->  [String]
split []    = []
split s     = clean (inSplit [] "" s) where
    inSplit        :: [String] -> String -> String -> [String]
    inSplit a cS "" = cS:a
    inSplit a cS (x:xs)
        | isAlpha x     = inSplit a (cS ++ [x]) xs
        | otherwise     = inSplit (cS:a) "" xs
    clean []        = []
    clean a         = [x | x <- a, not (null x)]

medellangd     :: String -> Double
medellangd []   = 0
medellangd s    = fromIntegral(sum [sum [1 | _ <- xs] | xs <- split s]) / fromIntegral(length (split s))


{- too slow but works
skyffla     :: [a] -> [a]
skyffla []   = []
skyffla all  = let
    inSkyffla          :: Int -> [a] -> [a] -> [a] -> [a]
    inSkyffla i a [] [] = a
    inSkyffla i a [] rA = inSkyffla 0 a rA []
    inSkyffla i a (x:xs) rA
        | even i    = inSkyffla (i-1) (a++[x]) xs rA
        | otherwise = inSkyffla (i-1) a xs (rA++[x])
    in inSkyffla (length all - 1) [] all [] -}



skyffla     :: [a] -> [a]
skyffla []   = []
skyffla all  = let
    inSkyffla              :: [a] -> [a] -> [a] -> [a]
    inSkyffla a [] []               = reverse a
    inSkyffla a [] rA               = inSkyffla a (reverse rA) []
    inSkyffla a (f:s:xs) rA         = inSkyffla (f:a) xs (s:rA)
    inSkyffla a (f:xs) rA           = inSkyffla (f:a) (reverse rA) []
    in inSkyffla [] all []


{- All code below this point is just to make it clear to myself  what is happening
    inSkyffla [] [1,2,3,4] []   -->
    inSkyffla [1] [3,4] [2]     -->
    inSkyffla [3,1] [] [4,2]    -->
    inSkyffla [3,1] [4,2] []    -->
    inSkyffla [2,3,1] [] [4]    -->
    inSkyffla [2,3,1] [4] []    -->
    inSkyffla [4,2,3,1] [] []   -->

    
    inSkyffla [] [1,2,3,4,5] [] 
    inSkyffla [1] [3,4,5] [2]       
    inSkyffla [1,3] [5] [2,4]
    inSkyffla [1,3,5] [5] [2,4]
-}

{- doesn't work
skyffla     :: [a] -> [a]
skyffla []   = []
skyffla all  = let
    inSkyffla              :: Int -> Int -> [a] -> [a]
    inSkyffla i m all@(x:xs)
        | m == -1    = all
        | i == m+1    = inSkyffla 0 m all
        | even i    = inSkyffla (i+1) (m-1) (xs++[x])
        | otherwise = inSkyffla (i+1) (m-1) (xs++[all!!i])
    in inSkyffla 0 (length all-1) all -}

    -- inSkyffla 0 3 [1,2,3,4] --> inSkyffla 1 2 [2,3,4,1]
    -- inSkyffla 1 2 [2,3,4,1] --> inSkyffla 2 1 [2,4,1,3]
    -- inSkyffla 2 1 [2,3,4,1] --> inSkyffla 0 1 [2,4,1,3]
    -- inSkyffla 0 1 [2,3,1,3] --> inSkyffla 1 0 [4,1,3,2]
    -- inSkyffla 1 0 [4,1,3,2] --> inSkyffla 0 0 [4,1,3,2]
    -- inSkyffla 0 0 [4,1,3,2] --> inSkyffla 1 -1 [1,3,2,4]

    -- inSkyffla 0 3 [1,2,3,4] --> inSkyffla 1 2 [2,3,4,1]
    -- inSkyffla 1 2 [2,3,4,1] --> inSkyffla 2 2 [2,3,4,1]
    -- inSkyffla 2 2 [2,3,4,1] --> inSkyffla 3 2 [2,4,1,3]