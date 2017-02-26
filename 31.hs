module Problem31 where

import Data.List

ways :: Integer -> Integer -> Integer
ways k c = sum . map (\(i, j) -> if k == i then 1 else ways (k - i) i) $ filter (\(x, y) -> x <= c) $ dropZeroes $ calcDivs k

calcDivs :: Integer -> [(Integer, Integer)]
calcDivs n = map (\x -> (x, n `div` x)) [1,2,5,10,20,50,100,200]

dropZeroes :: [(Integer, Integer)] -> [(Integer, Integer)]
dropZeroes = filter (\(x, y) -> y > 0)
