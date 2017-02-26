module Problem45 where

import Data.Maybe
import Math.NumberTheory.Powers.Squares (isSquare', exactSquareRoot)

-- Given an array of n, where n is the nth hexagonal number, returns all
-- of the hexagonal numbers that are also triangular and pentagonal
solve :: [Int] -> [Int]
solve = filter isTriangular . filter isPentagonal . map hexagonal

hexagonal :: Int -> Int
hexagonal n = (n * (2 * n - 1))

isTriangular :: Int -> Bool
isTriangular t = isSquare' (8 * t + 1)

isPentagonal :: Int -> Bool
isPentagonal p = if isJust x then
                             (fromJust x) `mod` 6 == 5
                             else
                             False
 where x = exactSquareRoot (24 * p + 1)
