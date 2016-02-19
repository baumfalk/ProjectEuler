
genListToSqrtN :: Integer -> [Integer]
genListToSqrtN n = [2 .. sqN]
  where n' = fromIntegral n
        sqN = (toInteger.floor. sqrt) n'
        
isPrime :: Integer -> Bool     
isPrime 2 = True   
isPrime n = (not . or . map checkFunc) list
  where list =  genListToSqrtN n
        checkFunc = \i  -> n `mod` i == 0
        
primes = filter isPrime (2:[3,5..])
        
num = 600851475143
getPrimeFactors :: Integer -> [Integer]
getPrimeFactors n = getPrimeFactors' n primes

getPrimeFactors' :: Integer -> [Integer] -> [Integer]        
getPrimeFactors' 0 _ = []
getPrimeFactors' 1 _ = []
getPrimeFactors' n curPL  | nModP = sP : getPrimeFactors' n' curPL'
                          | otherwise = getPrimeFactors' n curPL'
  where sP = head curPL
        curPL' = tail curPL
        nModP = n `mod` sP == 0
        n' = div n sP

pe003 = (maximum.getPrimeFactors) num
main = print pe003