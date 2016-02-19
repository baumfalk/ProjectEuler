import Data.List

numL :: [Integer]
numL = [100 .. 999]
listProduct :: [Integer]
listProduct = (reverse . sort) [x * y | x <- numL, y <- numL]

isP :: Integer -> Bool
isP n = ls == reverse ls
  where ls = isP' n

isP' :: Integer -> [Integer]

isP' n | n < 10 = [n]
       | otherwise = digit : (isP' n')
  where digit = n `mod` 10
        n' = div n 10
pe004 :: Integer
pe004 = (maximum.dropWhile (not.isP)) listProduct
main = print pe004