coins = [1,2,5,10,20,50,100,200]
amount = 200
-- naive
-- distr = [(a,b,c,d,e,f,g,h) | a <- [0.. (div amount (coins!!0))]
                -- ,b <- [0.. (div amount (coins!!1))]
                -- ,c <- [0.. (div amount (coins!!2))]
                -- ,d <- [0.. (div amount (coins!!3))]
                -- ,e <- [0.. (div amount (coins!!4))]
                -- ,f <- [0.. (div amount (coins!!5))]
                -- ,g <- [0.. (div amount (coins!!6))]
                -- ,h <- [0.. (div amount (coins!!7))]
                -- , a*coins!!0 
                -- + b*coins!!1 
                -- + c*coins!!2
                -- + d*coins!!3
                -- + e*coins!!4
                -- + f*coins!!5
                -- + g*coins!!6
                -- + h*coins!!7
                -- == amount]

distr :: Integer -> [Integer] -> [[Integer]]

distr 0 _ = [[]]
distr _ [] = []
distr amount coins@(c:cs) 
    | c <= amount = map (c:) (distr (amount - c)  coins) ++ (distr amount cs)
    | otherwise = (distr amount cs) 
                
pe031 :: Int
pe031 = length $ distr amount coins
main = print pe031

