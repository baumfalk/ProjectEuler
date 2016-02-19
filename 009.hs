pythTriples = [(a,b,c) | c <- [3..], b <- [2..(c-1)], a <- [1..(b-1)],a^2+b^2==c^2]
posDiv = [(a,b,c) | c <- [3..], b <- [2..(c-1)], a <- [1..(b-1)], a+b+c == 1000]


pyth :: (Integer, Integer, Integer) -> Bool
pyth (a,b,c) = a^2 + b^2 == c^2


prodTriple :: (Integer, Integer, Integer) -> Integer
prodTriple (a,b,c) = a*b*c


pe009 :: Integer
--pe009 = prodTriple $ head $ filter (\(a,b,c) -> a+b+c==1000) pythTriples
pe009 = prodTriple $ head $ filter (\(a,b,c) -> a^2+b^2 == c^2) posDiv
main = print pe009