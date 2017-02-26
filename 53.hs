module Problem53 where

solve :: [Integer] -> [Integer]
solve = filter (>1000000) . concat . map pascal

pascal :: Integer -> [Integer]
pascal l = [x | r <- [0..l],
               let n = [(l - r + 1)..l],
               let x = ((product n) `div` (product [1..r]))]
