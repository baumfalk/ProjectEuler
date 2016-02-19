numbers = [1..999]
divNum = [3,5]

divFunc = map (\x y-> y `mod` x)

mapF x = map (\y -> y x == 0) (divFunc divNum)

pe001 = sum $ map (\x ->  if (or . mapF) x then x else 0 ) numbers

--divisibleByThreeOrFive = (\x -> (x ` mod` 5 == 0) || (x ` mod` 3 == 0))


--main = print result
   --where result = (sum.(filter divisibleByThreeOrFive )) numbers
   
main = print pe001