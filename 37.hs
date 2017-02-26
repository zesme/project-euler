module Problem37 where

import Data.List
import Data.FastDigits
import Data.Numbers.Primes

solve :: Integer
solve = sum $ take 11 $ filter (\x -> and $ map (\y -> isPrime' y p) $ getCombinations x) $ map toInteger $ filter (>=10) p
  where p = 1:primes

--solve' :: [[Int]]
--solve' = map (\y -> map (\x -> isPrime' x p) (getCombinations y)) $ map toInteger $ filter (>=10) p
--  where p = 1:primes

isPrime' :: Int -> [Int] -> Bool
isPrime' 1 _ = False
isPrime' x p = x `elem` (takeWhile (<=x) p)

-- Generate combinations
getCombinations :: Integer -> [Int]
getCombinations x = (subNumbers x) ++ (tail $ subNumbers' x)

-- Removing from the right
subNumbers :: Integer -> [Int]
subNumbers x
  | x <= 0    = []
  | otherwise = (fromInteger x):(subNumbers (undigits 10 $ tail $ digits 10 x))

-- Removing from the left
subNumbers' :: Integer -> [Int]
subNumbers' x
  | x <= 0    = []
  | otherwise = (fromInteger x):(subNumbers' (undigits 10 $ reverse $ tail $ reverse $ digits 10 x))
