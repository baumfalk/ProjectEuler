import Prime


belowNum = 2000000
pe010 :: Int
pe010 = (sum. primesToNA) belowNum


--pe010 = sum $ filter (<belowNum) primes
main = print pe010