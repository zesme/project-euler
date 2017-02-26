module Main where

import System.Environment (getArgs)

import Data.List
import Data.FastDigits (digits)

-- Takes a list of primes separated by newlines and chceks them for pandigitality
-- Run as such: > primesieve 987654321 -p | ./41
main = do
  primeList <- getContents
  putStr $ show $ maximum $ solve $ map (\x -> read x :: Int) $ lines primeList

solve :: [Int] -> [Int]
solve = filter (isPandigital)

isPandigital :: Int -> Bool
isPandigital x = and $ map (\x -> if length (elemIndices x d) == 1 then True else False)
  [1..(ceiling (10 `logBase` (fromIntegral x)))]
  where d = digits 10 (toInteger x)
