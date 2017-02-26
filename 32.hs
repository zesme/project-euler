module Problem32 where

import Data.List

solve :: Integer -> Integer
solve r = sum $ removeDuplicates $ foldr (\x y -> y ++ x) [] $ filter (not . null) $ map solveString $ permutations $ show r

solveString :: String -> [Integer]
solveString s = map (\(_, _, a) -> Problem32.toInteger a) $ filter trySolve $ map (\c -> parse s c) (relativeConfig $ configurations [1..8])

trySolve :: (String, String, String) -> Bool
trySolve (x, y, z) = x' * y' == z'
  where x' = Problem32.toInteger x
        y' = Problem32.toInteger y
        z' = Problem32.toInteger z

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

configurations :: Ord a => [a] -> [(a, a)]
configurations a = filter (\(x, y) -> x < y) $ cartProd a a

relativeConfig :: Num a => [(a, a)] -> [(a, a)]
relativeConfig = map (\(x, y) -> (x, y - x))

parse :: String -> (Integer, Integer) -> (String, String, String)
parse s (a, b) = (x, y, z)
  where (x, xs) = splitAt (fromInteger a) s
        (y, z) = splitAt (fromInteger b) xs

toInteger :: String -> Integer
toInteger n = read n :: Integer

removeDuplicates :: Ord a => [a] -> [a]
removeDuplicates = map head . group . sort
