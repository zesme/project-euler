module Problem50 where

import Data.Maybe
import Data.List (sortBy)
import Data.Numbers.Primes (primes)

-- Gets the longest prime that can be a consecutive sum
--solve :: [Integer] -> Int
solve = filter (\(_, x) -> x > 0) . sortBy sort' . map (\x -> (x, test x))

sort' :: Ord a => (b, a) -> (b, a) -> Ordering
sort' (_,a) (_,b)
  | a < b = GT
  | b > b = LT
  | otherwise = EQ

test :: Integer -> Int
test n = if (length n') > 0 then maximum n' else 0
  where n' = catMaybes $ map (\x -> chainLength n x) $ takeWhile (\x -> (length x) >= 0) $ iterate (tail) $ reverse $ takeWhile (<(n `div` 2)) primes

chainLength :: Integer -> [Integer] -> Maybe Int
chainLength n a = if last k == 0 then Just ((length k) - 1) else Nothing
  where k = takeWhile (>=0) $ scanl (\x y -> x - y) n a
