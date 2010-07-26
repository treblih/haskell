{- ================ pe 1 ================ -}
pe1 :: Int
pe1 = sum [x | x <- [1..999], x `mod` 3 == 0 || x `mod` 5 == 0]

{- ================ pe 2 ================ -}
fib 0 = 0
fib 1 = 1
fib n | even n         = f1 * (f1 + 2 * f2)
      | n `mod` 4 == 1 = (2 * f1 + f2) * (2 * f1 - f2) + 2
      | otherwise      = (2 * f1 + f2) * (2 * f1 - f2) - 2
   where k = n `div` 2
         f1 = fib k
         f2 = fib (k-1)

fib_list :: Int -> [Int]
fib_list 0 = []
fib_list 1 = [1]
fib_list n = fib_list (n - 1) ++ [fib n]

-- fibs n = a
-- 	where a = array (1, n) ([(1, 1), (2, 1)] ++ [(i, a!(i - 1) + a!(i - 2)) | i <- [3..n]])

fibs :: [Int]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
-- fibs = 1 : scanl (+) 1 fibs

pe2 :: Int
pe2 = sum $ takeWhile (<= 4 * 10 ^ 6) [x | x <- fibs, even x]

{- ================ pe 3 ================ -}
