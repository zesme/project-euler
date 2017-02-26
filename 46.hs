module Problem46 where

import Data.List (intersectBy)
import Data.Numbers.Primes (primes, isPrime)
import Math.NumberTheory.Powers.Squares (isSquare')

solve :: [Int] -> [Int]
solve = filter (not . test)

composites :: [Int]
composites = filter (not . isPrime) [3,5..]

-- Tests where a given odd number fulfills Goldbach's other conjecture
-- No necessity for [even (b - a)] and b - a is always even for b > 2,
-- b prime and a odd
test :: Int -> Bool
test n = or $ map (\(a, b) -> isSquare' ((b - a) `div` 2)) $
  zip (takeWhile (<n) primes) (repeat n)
