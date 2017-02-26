module Problem29 where

import Data.List

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = map head . group . sort

solve :: [Integer] -> [Integer] -> Int
solve a b = length $ removeDuplicates $ map (\(x,y) -> x^y) $ cartProd a b
