module Problem35 where

import Data.List
import Data.Numbers.Primes
import qualified Data.Set as Set

solve :: Int -> Int
solve n = length $ filter (\x -> isCircular x p) p
  where p = takeWhile (<n) primes

isCircular :: Int -> [Int] -> Bool
isCircular r p = foldr (\x y -> if x && y then True else False) True $ map (\x -> (read x :: Int) `elem` p) $
  rotate $ show r

rotate :: [a] -> [[a]]
rotate [] = []
rotate x = map (\a -> take (length x) a) [drop o (cycle x) | o <- [0..((length x) - 1)]]

-- Unnecessary, but kept here for reference. Good method for duplicate removal.
-- Probably available in a Hackage somewhere.
nubOrd :: Ord a => [a] -> [a]
nubOrd xs = go Set.empty xs where
  go s (x:xs)
   | x `Set.member` s = go s xs
   | otherwise        = x : go (Set.insert x s) xs
  go _ _              = []
