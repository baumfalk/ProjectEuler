module Prime where

genListToSqrtN :: Integer -> [Integer]
genListToSqrtN n = [2 .. sqN]
  where n' = fromIntegral n
        sqN = (toInteger.floor. sqrt) n'

isPrime :: Integer -> Bool
isPrime n | n <= 1 = False
isPrime 2 = True   
isPrime n = (not . or . map checkFunc) list
  where list =  genListToSqrtN n
        checkFunc = \i  -> n `mod` i == 0

isP = isPrime        
        
primes = filter isPrime (2:[3,5..])

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

     
     
getAPF = getAllPrimeFactors     
getAllPrimeFactors :: Integer -> [Integer]
getAllPrimeFactors n = getAllPrimeFactors' n primes

getAllPrimeFactors' :: Integer -> [Integer] -> [Integer]        
getAllPrimeFactors' 0 _ = []
getAllPrimeFactors' 1 _ = []
getAllPrimeFactors' n curPL  | nModP = sP : getAllPrimeFactors' n' curPL
                          | otherwise = getAllPrimeFactors' n curPL'
  where sP = head curPL
        curPL' = tail curPL
        nModP = n `mod` sP == 0
        n' = div n sP