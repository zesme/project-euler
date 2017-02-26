module Main where

import Data.String.Utils (replace, split)
import Data.Char (ord)

main = do
  raw <- getContents
  putStr $ show $ solve raw

solve :: String -> Int
solve = length . filter (isTriangular) . map getValue . parse
  where t = triangularNumbers [1..]

parse :: String -> [String]
parse = map (\x -> replace "\"" "" x) . split ","

getValue :: String -> Int
getValue = foldr (\x y -> y - 64 + ord x) 0

triangularNumbers :: [Int] -> [Int]
triangularNumbers = map (\n -> (n * (n + 1)) `div` 2)

isTriangular :: Int -> Bool
isTriangular x = x `elem` t
  where t = takeWhile (<=x) $ triangularNumbers [1..]
