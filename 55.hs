module Problem55 where

import Data.FastDigits (digits, undigits)

-- Counts all lycharel numbers (up to 50 iterations) in a given range
-- Solution: > solve [1..9999]
solve :: [Integer] -> Int
solve = length . filter isLycharel

isLycharel :: Integer -> Bool
isLycharel = not . or . map isPalindrome' . take 50 . tail . iterate reverseAdd

reverseAdd :: Integer -> Integer
reverseAdd x = x + (undigits 10 $ reverse $ digits 10 x)

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

isPalindrome' :: Integer -> Bool
isPalindrome' = isPalindrome . digits 10
