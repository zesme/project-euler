module Problem44 where

import Data.Maybe
import Math.NumberTheory.Powers.Squares (exactSquareRoot)

suitable :: Int -> [(Int, Int)]
suitable r = [(x', y') | x <- [1..r],
                         y <- [(x+1)..r],
                         let x' = pentagonal x,
                         let y' = pentagonal y,
                         isPentagonal (y' - x'),
                         isPentagonal (x' + y')]

pentagonal :: Int -> Int
pentagonal n = (n * (3 * n - 1)) `div` 2

isPentagonal :: Int -> Bool
isPentagonal p = if isJust x then
                             (fromJust x) `mod` 6 == 5
                             else
                             False
 where x = exactSquareRoot (24 * p + 1)
