-- from Autrijus Tang
-- print first 100 hamming numbers
main = print (take 100 hamming)
hamming :: [Int]
hamming = 1 : (map (2*) hamming) ~~ (map (3*) hamming) ~~ (map (5*) hamming)
    where
    xxs@(x:xs) ~~ yys@(y:ys) -- To merge two streams:
        | x == y = (x : xs ~~ ys) --   if the heads are common, take that
        | x < y = x:xs ~~ yys -- !!!!!! 
        | otherwise = (y : xxs ~~ ys)   -- and proceed to merge the rest
