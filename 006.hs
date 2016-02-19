nums = [1 .. 100]
numSum = sum nums
numSumSq = numSum ^2
numSqSum = sum [x*x | x <- nums]

pe006 :: Integer
pe006 = numSumSq - numSqSum

main = print pe006