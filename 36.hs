module Problem36 where

import Data.List
import Numeric (showHex, showIntAtBase)
import Data.Char (intToDigit)

solve :: [Integer] -> Integer
solve = sum . filter (\x -> (isPalindrome $ show x) && (isPalindrome $ toBinary x))

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

toBinary :: Integer -> [Char]
toBinary x = showIntAtBase 2 intToDigit x ""
