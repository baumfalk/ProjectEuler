module Prime where

import Data.Array.Unboxed
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

primes :: [Integer]
primes = filter isPrime (2:[3,5..])

primesToNA n = 2: [i | i <- [3,5..n], ar ! i]
  where
    ar = f 5 $ accumArray (\ a b -> False) True (3,n) 
                        [(i,()) | i <- [9,15..n]]
    f p a | q > n = a
          | True  = if null x then a2 else f (head x) a2
      where q = p*p
            a2  :: UArray Int Bool
            a2 = a // [(i,False) | i <- [q, q+2*p..n]]
            x  = [i | i <- [p+2,p+4..n], a2 ! i]


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