{- ================ 1 ================ -}
last' :: [a] -> a
last' [] = error "empty"
last' [x] = x
last' (x:xs) = last' xs
-- last' = foldr1 (const id)
-- last' = head . reverse

{- ================ 2 ================ -}
last_but_one :: [a] -> a
last_but_one [] = error "empty"
last_but_one [x, _] = x
last_but_one (x:xs) = last_but_one xs
-- last_but_one = last . init
-- last_but_one xs = reverse xs !! 1

{- ================ 3 ================ -}
find_kth :: [a] -> Int -> a
find_kth xs i
    | i <= len = xs !! (i-1)
    | otherwise = error "bounds exceeded"
    where
    len = length xs

{- ================ 4 ================ -}
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

{- ================ 5 ================ -}
reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []
--reverse' = foldr (\x acc -> acc ++ [x]) []

{- ================ 6 ================ -}
palindrome :: Eq a => [a] -> Bool
palindrome [] = True
palindrome [x] = True
palindrome (x:xs)
    | x == last xs = palindrome . init $ xs
    | otherwise = False
-- palindrome' xs = xs == (reverse xs)

{- ================ 7 * ================ -}
data Nest a = Elem a | List [Nest a]
flatten :: Nest a -> [a]
flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

{- ================ 8 * ================ -}
compress :: Eq a => [a] -> [a]
compress = compress' [] . reverse
    where
    compress' acc [] = acc
    compress' [] (x:xs) = compress' [x] xs
    compress' acc (x:xs)
        | x /= head acc = compress' (x:acc) xs
        | otherwise =  compress' acc xs
-- compress = map head . group
  
{- ================ 9 * ================ -}
group :: Eq a => [a] -> [[a]]
group = foldr f []
    where
    f x [] = [[x]]
    f x (a:as)
        | x == head a = ((x:a):as)
        | otherwise = ([x]:a:as)
{- group' [] = []
group' (x:xs) = let (first, rest) = span (==x) xs
                in first : group' rest -}

{- ================ 10 ================ -}
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . group
-- encode xs = [(length x, head x) | x <- group xs]
-- encode = map (length &&& head) . group
-- encode = let f x acc = (length x, head x) : acc
         -- in foldr f [] . group
