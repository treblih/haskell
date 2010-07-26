import Array

length' :: [a] -> Int   
-- length' [] = 0   
-- length' (_:xs) = 1 + length' xs
length' xs = sum [1 | _ <- xs]

remove_lowercase st = [c | c <- st, c `elem` ['A'..'Z']]

add_vectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
add_vectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

tell :: [a] -> String
tell [] = "The list is empty"   
tell (x:[]) = "The list has one element: " 
tell (x:y:[]) = "The list has two elements: "
tell (x:y:_) = "This list is long. The first two elements are: "

head' :: [a] -> a   
head' [] = error "Can't call head on an empty list, dummy!"   
head' (x:_) = x 

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
	| n <= 0 = []
take' _ [] = []
take' n (x : xs) = x : take' (n-1) xs

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [a] -> [(a, a)]
zip' [] _ = []
zip' _ [] = []
zip' (a:x) (b:y) = (a, b) : zip' x y

elem' :: (Eq a) => a -> [a] -> Bool		
elem' _ [] = False
elem' n (x:xs)
	| n == x = True
	| otherwise = elem' n xs

findsmall :: (Ord a) => a -> [a] -> [a]
findsmall _ [] = []
findsmall n (x:xs)
	| n > x = x:findsmall n xs
	| otherwise = findsmall n xs

findlarge :: (Ord a) => a -> [a] -> [a]
findlarge _ [] = []
findlarge n (x:xs)
	| n <= x = x:findlarge n xs
	| otherwise = findlarge n xs

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) = quicksort' (findsmall x xs) ++ [x] ++ quicksort' (findlarge x xs)

fac ::Int->Int
fac 0 = 1
fac n = fac (n-1) * n

zipWith' :: (a -> a -> a) -> [a] -> [a] -> [a]   
zipWith' _ [] _ = []   
zipWith' _ _ [] = []   
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> a -> a) -> (a -> a -> a)   
flip' = \f -> \x -> \y -> f y x

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
	| odd n = n : collatz (3 * n + 1)
	| even n = n : collatz (n `div` 2)

map' :: (a -> a) -> [a] -> [a]
map' f xs = foldr (\x acc -> f x : acc) [] xs

sum' = foldl1 (+)

maxmium' :: (Ord a, Num a) => [a] -> a
-- all are ok
--maxmium' = foldr (\x acc -> if x > acc then x else acc) 0
--maxmium' = foldr1 (\x acc -> if x > acc then x else acc)
maxmium' = foldl (\acc x -> if x > acc then x else acc) 0

reverse' :: [a] -> [a]
--reverse' = foldr (\x acc -> acc ++ [x]) []
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr (*) 1

filter' :: (Ord a) => (a -> Bool) -> [a] -> [a]
filter' f = foldr (\x acc -> if f x then x : acc else acc) []

head'' :: [a] -> a
head'' = foldl1 (\acc x -> x)

mem0 :: String -> Bool
mem0 str = or $ map (== '0') str

-- map toUpper "abc" = foldr ((:) . toUpper) [] "abc"
-- elem 3 [1, 2, 3, 4] = foldr ((||) . (== 3)) False [1, 2, 3, 4]

--data Main.Maybe a = Main.Nothing | Main.Just a
--show_maybe :: (Show a) => Maybe a -> String
--show_maybe Nothing = []
--show_maybe (Just a) = show a

occurence :: Eq a => a -> [a] -> Int
--occurence n xs = length $ foldr (\x acc -> if x == n then x : acc else acc) [] xs
occurence _ [] = 0
occurence n (x : xs)
	| n == x = 1 + occurence n xs
	| otherwise = occurence n xs

remove_occur :: Eq a => a -> [a] -> [a]
--remove_occur n xs = foldr (\x acc -> if x == n then acc else x : acc) [] xs
remove_occur _ [] = []
remove_occur n (x : xs)
	| n == x = remove_occur n xs
	| otherwise = x : remove_occur n xs

--[e | e <- [1..20], [x | x <- [1..e], x*x == e] == []]

copy :: [a] -> [a]
copy [] = []
copy (x : xs) = x : copy xs

iverse :: [(a, a)] -> [(a, a)]
iverse [] = []
iverse ((a, b) : xs) = (b, a) : iverse xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] b = b
merge a [] = a
merge (a1 : ax) (b1 : bx)
	| a1 < b1 = a1 : merge ax (b1 : bx)
	| otherwise = b1 : merge bx (a1: ax) 

data May a = No | Ju a deriving Show
get' :: Num n => [a] -> n -> May a
get' [] _ = No
get' xs 1 = Ju $ head xs
get' (x : xs) n = get' xs (n - 1)

look_dic :: Eq a => [(a, b)] -> a -> Maybe b
look_dic [] _ = Nothing
-- 			the 2 lines all works ok respectively
--look_dic ((a, b) : xs) wanted = if a == wanted then Just b else look_dic xs wanted
--look_dic (x : xs) wanted = if fst x == wanted then Just $ snd x else look_dic xs wanted
look_dic ((key, val) : xs) wanted
	| key == wanted = Just val
	| otherwise = look_dic xs wanted

{- indirect recursion -}
rm_alter :: [a] -> [a]
rm_alter [] = []
rm_alter (x : xs) = indirect xs

indirect :: [a] -> [a]
indirect [] = []
indirect (x : xs) = x : rm_alter xs
{- end -}

extract :: [Maybe a] -> [a]
extract [] = []
extract (Nothing : xs) = extract xs
extract (Just x : xs) = x : extract xs

{- whether String b is inner String a -}
-- if want to use a STATE, make another arg
auxi :: String -> String -> Int -> Maybe Int
auxi [] _ _ = Nothing
auxi _ [] _ = Nothing
auxi (a : xa) (b : xb) n
	| a /= b = auxi xa (b : xb) (n + 1)
	| otherwise = 
		if xb == [] 
		then Just n
		else auxi xa xb n
		-- | xb == [] = Just n
		-- | otherwise = auxi xa xb n

find_inner :: String -> String -> Maybe Int
find_inner a b = auxi a b 0
{- end -}

fold_with :: (a -> a -> a -> a) -> a -> [a] -> [a] -> a
fold_with _ acc [] _ = acc
fold_with _ acc _ [] = acc
fold_with f acc (a : xa) (b : xb) = f a b (fold_with f acc xa xb)

is_exist :: Eq a => a -> [a] -> Bool
is_exist _ [] = False
is_exist n (x : xs)
	| n == x = True
	| otherwise = is_exist n xs

intersection :: Eq a => [a] -> [a] -> [a]
intersection _ [] = []
intersection [] _ = []
intersection list (x : xs)
	| elem x list = x : intersection list xs
	| otherwise = intersection list xs

is_subset :: Eq a => [a] -> [a] -> Bool
is_subset _ [] = False
is_subset [] _ = True
is_subset (x : xs) mother
	| elem x mother = is_subset xs mother
	| otherwise = False

pairs :: [a] -> [(a, a)]
pairs xs = zip xs $ tail xs

is_sorted :: Ord a => [a] -> Bool
is_sorted xs = and [x <= y | (x, y) <- pairs xs]
{-
is_sorted [] = True
is_sorted (x : xs)
	| length xs == 0 = True
	| otherwise = if x <= head xs
			then is_sorted xs
			else False
-}

last' :: [a] -> Maybe a
last' [] = Nothing
last' (x : xs)
	| length xs == 0 = Just x
	| otherwise = last' xs

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

tree1 = Node
		(Node (Leaf 4) 2 (Leaf 5))
		1
		(Node (Leaf 6) 3 (Leaf 7))

-- tree2 = Node 1
-- 		Leaf
-- 		(Node 2
-- 		 	Leaf
-- 		 	(Node 3
-- 			 	Leaf
-- 			 	(Node 4
-- 				 	Leaf
-- 					(Node 5
-- 					 	Leaf
-- 						(Node 6
-- 						 	Leaf
-- 							(Node 7
-- 							 	Leaf
-- 								Leaf))))))

data Exp = Const Int | Add Exp Exp | Multi Exp Exp deriving Show
eval :: Exp -> Int
eval (Const n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Multi e1 e2) = eval e1 * eval e2

data Pair' a b = Pair' a b
pair1st (Pair' a b) = a

data Triple a b c = Triple a b c
triple1st (Triple a b c) = c

data Quadruple a b = Quadruple a a b b
aaa :: Quadruple a b -> [a]
aaa (Quadruple a b c d) = [a, b]

data Tuple a b c d = One a | Two a b | Three a b c | Four a b c d
tuple1 :: Tuple a b c d -> Maybe a
-- tuple1 (One []) = Nothing
tuple1 (One a) = Just a

tuple2 :: Tuple a b c d -> Maybe b
-- tuple2 (Two _ []) = Nothing
tuple2 (Two _ b) = Just b

fromTuple (One   a      ) = Left  (Left  a        )
fromTuple (Two   a b    ) = Left  (Right (a,b)    )
fromTuple (Three a b c )  = Right (Left  (a,b,c) )
fromTuple (Four  a b c d) = Right (Right (a,b,c,d))

data List a = Nil | Cons a (List a)
list_head :: List a -> Maybe a
list_head Nil = Nothing
list_head (Cons x _) = Just x

{-
list_foldl :: (a->a) -> acc -> (List a) -> acc
list_foldl _ acc Nil = acc
list_foldl f acc (Cons x xs) = list_foldl f (f x) xs
-}

listFoldl :: (acc -> a -> acc) -> acc -> List a -> acc
listFoldl f y Nil = y
listFoldl f y (Cons x xs) = listFoldl f (f y x) xs

list_foldr :: (a -> a -> a) -> a -> List a -> a
list_foldr f a Nil = a
list_foldr f a (Cons x xs) = f x (list_foldr f a xs)

inorder :: Tree a -> [a]
inorder (Leaf x) = [x]
inorder (Node l x r) = inorder l ++ [x] ++ inorder r

-- inorderf :: (a -> a) -> Tree1 a -> [a]
-- inorderf f Leaf = []
-- inorderf f (Node x t1 t2) = inorderf f t1 ++ [f x] ++ inorderf f t2

fold_btree :: (a -> a -> a) -> a -> Tree a -> a
fold_btree f acc (Leaf x) = f x acc
fold_btree f acc (Node l x r) = fold_btree f (f x $ fold_btree f acc l) r

show' :: a -> a
show' a = a

concat' :: [[a]] -> [a]
concat' xss = [x | xs <- xss, x <- xs]

factor :: Int -> [Int]
factor n = 1 : [x | x <- [2..(div n 2)], mod n x == 0] ++ [n]

(###) :: Int -> Int
(###) a = a + 1

all_empty n
	| otherwise = False
	| n `div` 2 == 0 = True


sum_square :: Num a => [a] -> a
sum_square [] = 0
sum_square (x:xs) = x * x + sum_square xs

find' :: [Int] -> [Int]
find' n = takeWhile (<= 100) [x * x | x <- n]

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes list@(x:xs) = list : suffixes xs

permutation [] = [[]]
permutation xs = [x:p | x <- xs, p <- permutation $ rem_fst x xs]
	where rem_fst x [] = []
	      rem_fst x (y:ys)
	      	| x == y = ys
		| otherwise = y : rem_fst x ys


fibs n = a
	where a = array (1, n) ([(1, 1), (2, 1)] ++ [(i, a!(i - 1) + a!(i - 2)) | i <- [3..n]])

compose f g x = f (g x)

neg :: [Int] -> Int
neg xs = sum [1 | x <- xs, x < 0]

ttt = array ((1,1),(3,3))
         [(ij,v) | (ij,v) <- zip [(i, j) | i<-[1..3],j<-[1..3]]
         -- [((i, j),v)|((i, j),v)<-zip [(i,j)|i<-[1..3],j<-[1..3]]
                             [2..10]]

transpose3x3 a = array ((1,1),(3,3)) [((i,j),a!(j,i))|i<-[1..3],j<-[1..3]]

sumAtoB a b = sum [a..b]

splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in  pre : case suf of 
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []

isLineTerminator c = c == '\r' || c == '\n'

str2action :: String -> IO ()
str2action input = putStrLn ("Data: " ++ input)

tabspace = 4
tabstops = map (\col -> col `mod` tabspace == 1) [ 1 .. ]

tabspaces :: [Bool] -> [Int]
tabspaces ts = spacesToTabstop tablessPrefix ++ tabspaces rest'
    where
      (tablessPrefix, (_:rest)) = span (\stop -> not stop) ts
      rest' = False:rest
      spacesToTabstop prefix = reverse [1 .. (length prefix)]

foldl' :: (a -> a -> a) -> a -> [a] -> a
foldl' f acc [] = acc
foldl' f acc (x:xs) = acc `seq` foldl' f acc xs
