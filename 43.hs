module Problem43 where

import Data.List (elemIndices, permutations)
import Data.FastDigits (digits, undigits)

solve :: Integer
solve = sum $ filter isDivisible $ map (\x -> undigits 10 x) $ filter (\x -> last x > 0) $ permutations [0..9]

isDivisible :: Integer -> Bool
isDivisible n = and $ map (\(a, b) -> b `mod` p !! a == 0) $ tail $
  map (\(a, b) -> (a, undigits 10 $ reverse $ take 3 $ drop a b)) $
  zip [0..] $ replicate 8 $ reverse $ digits 10 n
    where p = [1,2,3,5,7,11,13,17]
