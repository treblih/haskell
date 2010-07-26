a1 = foldr1 (const id)
-- a1 = head . reverse

a2 = head . reverse . init
a2' = head . tail . reverse

a3 :: [a] -> Int -> Maybe a
a3 [] _ = Nothing
a3 (x:xs) 0 = Just x
a3 xs n = Just $ xs !! (n - 1)

a4 :: [a] -> Int
a4 [] = 0
a4 xs = sum [1 | x <- xs]
-- a4 (_:xs) = 1 + a4 xs
-- a4 xs = foldr (\x acc -> acc + 1) 0
