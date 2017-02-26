module Problem56 where

import Data.FastDigits (digits)

solve :: [Int] -> [Int]
solve r = [getSum a b | a <- r, b <- r]

getSum :: (Integral a, Integral b) => a -> b -> Int
getSum a b = sum $ digits 10 $ toInteger ((toInteger a) ^ (toInteger b))

