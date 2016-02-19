fib 1 = 1
fib 2 = 2
fib n = fib (n-1) + fib (n-2)

-- fast fib from haskell.org
fib' = fst . fib2
 
-- | Return (fib n, fib (n + 1))
fib2 0 = (1, 1)
fib2 1 = (1, 2)
fib2 n
 | even n    = (a*a + b*b, c*c - a*a)
 | otherwise = (c*c - a*a, b*b + c*c)
 where (a,b) = fib2 (n `div` 2 - 1)
       c     = a + b

fibs = map fib' [1..]
maxNumber = 4000000

pe002 = sum . filter even . takeWhile (<=maxNumber) $ fibs
main = print pe002