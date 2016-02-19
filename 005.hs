import Prime
import Data.List
nums :: [Integer]
nums = [1 .. 20]

numPF :: [[Integer]]
numPF = map (getAPF) nums

pNums :: [Integer]
pNums = filter (isP) $ (nub.concat) numPF

count :: Eq a => a -> [a] -> Integer
count x =  toInteger . length . filter (==x)

-- returns a list with num of occurrences of x in the sublists
occInL :: Integer -> [[Integer]] -> [Integer]
occInL x nums = map (\y -> count x y) nums

-- returns the maximum number of occurrences of x in a lists of lists
getMaxListNum :: Integer -> [[Integer]] -> Integer
getMaxListNum num = (maximum. occInL num) 

--divByAllNum ::  [Integer] -> Integer -> Bool
--divByAllNum numL n  = (and . map (\x -> n `mod` x == 0)) numL

pe005 :: Integer
--pe005 = (head . dropWhile (not . divByAllNum nums)) [1.. product(nums)]
pe005 = (product .  map (\x -> (x^(getMaxListNum x numPF)))) pNums
main = print pe005

