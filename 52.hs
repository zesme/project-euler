module Problem52 where

import Data.FastDigits (digits)
import Data.List (sort)

solve :: [Integer]
solve = filter check [1..]

check :: Integer -> Bool
check n = and $ map (\x -> d == (digits' (x*n))) [2..6]
  where d = digits' n

digits' :: Integer -> [Int]
digits' = sort . digits 10
