module Problem39 where

import Data.List

solve :: Float -> Int
solve = fst . head . sortBy (sort') . map (\x -> (head x, length x)) . group . sort . genList

sort' :: (Int, Int) -> (Int, Int) -> Ordering
sort' (_,a) (_,b)
  | a < b = GT
  | b > b = LT
  | otherwise = EQ

genList :: Float -> [Int]
genList k = [p | a <- [1..k],
                 b <- [a..k],
                 let c = sqrt ((a ** 2) + (b ** 2)), isInt c,
                 let p = round (a + b + c), p <= 1000]

isInt :: Float -> Bool
isInt x = x == fromInteger (round x)
