import Data.List (tails, sortBy)
import Data.Ord (comparing)

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
                                 --
{- ================ 11 ================ -}
data Amount a = Single a | Multiple Int a deriving (Show)
encode' :: Eq a => [a] -> [Amount a]
encode' =  map (\x -> 
                if length x == 1 
                then Single (head x) 
                else Multiple (length x) 
               (head x)) . group

{- ================ 12 ================ -}
decode' :: Eq a => [Amount a] -> [a]
decode' = foldr f []
    where
    f (Single x) acc = x:acc
    f (Multiple n x) acc = replicate n x ++ acc
{- decode' = concatMap f
    where
    f (Single x) = [x]
    f (Multiple n x) = replicate n x -}

{- ================ 13 ================ -}

{- ================ 14 * ================ -}
double :: [a] -> [a]
double xs = (\x -> [x, x]) =<< xs   -- list monad
-- double (x:xs) = x : x : double xs
-- double xs = concat [[x, x] | x <- xs]
--
{- ================ 15 * ================ -}
repli :: [a] -> Int -> [a]
repli = flip $ concatMap . replicate
-- repli xs n = -> concatMap (replicate n) xs
--
{- ================ 16 * ================ -}
drop_every :: [a] -> Int -> [a]
{- drop_every xs 0 = xs
drop_every _ 1 = []
drop_every [] _ = []
drop_every xs n = take (n - 1) xs ++ drop_every (drop n xs) n -}
-- drop_every xs n = [x | (x, i) <- zip xs [1..n], i `mod` n /= 0]
drop_every xs n = map fst . filter ((n/=) . snd) $ zip xs (cycle [1..n])

{- ================ 17 ================ -}
split_at :: [a] -> Int -> ([a], [a])
-- split_at xs n = (take n xs, drop n xs)
split_at = flip $ splitAt

{- ================ 18 ================ -}
slice :: [a] -> Int -> Int -> [a]
slice xs start end = take (end - start + 1) . drop (start - 1) $ xs

{- ================ 19 ================ -}
rotate :: [a] -> Int -> [a]
rotate xs n
    | n < 0 = drop (length xs + n) xs ++ take (length xs + n) xs
    | otherwise = drop n xs ++ take n xs

{- ================ 20 ================ -}
remove_at :: Int -> [a] -> (a, [a])
remove_at n xs = (xs !! n, take n xs ++ drop (n + 1) xs)

{- ================ 21 ================ -}
insert_at :: a -> [a] -> Int -> [a]
insert_at x xs n = take (n - 1) xs ++ [x] ++ drop (n - 1) xs

{- ================ 22 * ================ -}
range' :: Int -> Int -> [Int]
range' start end = [start..end]
-- range' = enumFromTo
--
{- ================ 23 24 25 random ================ -}

{- ================ 26 ================ -}
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs
                           , ys <- combinations (n-1) xs']

{- ================ 28 ================ -}
len_sort :: [String] -> [String]
len_sort = sortBy (comparing length)
