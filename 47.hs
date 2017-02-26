module Problem47 where

import Data.Numbers.Primes (primeFactors)
import Data.List (intersect, group, sort)

solve :: [Int] -> [Int]
solve = filter distinctPrimes

distinctPrimes :: Int -> Bool
distinctPrimes = and . map (\x -> length x == 4) . map (\x -> map head x) .  map (group . primeFactors) .
  zipWith (\x y -> x + y) [0..3] . repeat

overlap :: Int -> Int -> Bool
overlap a b = not $ null $ (primeFactors a) `intersect` (primeFactors b)
