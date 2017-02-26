module Problem49 where

import Data.List (permutations, subsequences)
import Data.Numbers.Primes (primes, isPrime)
import Data.FastDigits (digits, undigits)

solve :: [[[Integer]]]
solve = filter (not . null) $ map (permutables) $ takeWhile (<10000) $ filter (>=1000) $ primes

permutables :: Integer -> [[Integer]]
permutables n = filter (\(a:(b:(c:[]))) -> b - a > 0 && b - a == c - b) $ filter (\x -> (length x) == 3) $ subsequences $
  filter (isPrime) $ map (\x -> undigits 10 x) $ permutations $ digits 10 n

