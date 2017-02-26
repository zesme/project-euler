module Problem40 where

import Data.List
import Data.Char (digitToInt)

solve :: [Int] -> Int
solve = foldr1 (*) . map (\x -> digitToInt $ champernowne !! (x - 1))

champernowne :: String
champernowne = foldr1 (++) $ map show [1..]
