module Problem38 where

import Data.List
import Data.FastDigits
import Data.Maybe (catMaybes)

solve :: [Int] -> Int
solve = maximum . filter (isPandigital) . catMaybes . map parse

isPandigital :: Int -> Bool
isPandigital x = and $ map (\x -> if length (elemIndices x d) == 1 then True else False) [1..9]
  where d = digits 10 (toInteger x)

parse :: Int -> Maybe Int
parse n = tryParse $ parsePossibility $ map (*n) [1..9]

tryParse :: [String] -> Maybe Int
tryParse s
  | (length n) > 1 = Just (last n)
  | otherwise      = Nothing
  where n = map (\x -> read x :: Int) s

parsePossibility :: [Int] -> [String]
parsePossibility = takeWhile ((<10) . length) . scanl1 (++) . map show
