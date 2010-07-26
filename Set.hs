module Set (
  Set,
  empty,
  is_empty,
  Set.elem,
  add,
  del
) where

import qualified List as L
import Prelude hiding (max)

{- list implementation
	
newtype Set a = Set [a]
instance Show a => Show (Set a) where
  showsPrec p (Set a) str = show_set a str

empty :: Set a
is_empty :: Set a -> Bool
elem :: (Eq a, Ord a) => a -> Set a -> Bool
add :: (Eq a, Ord a) => a -> Set a -> Set a
del :: (Eq a, Ord a) => a -> Set a -> Set a


empty = Set []

is_empty (Set []) = True
is_empty _ = False


{- unordered with duplication
add a (Set s) = Set (a:s)
del a s = Set (filter (/= a) s)
-}
	
-- elem a (Set xs) = L.elem a xs

{- unordered without duplication
add a s@(Set xs)
  | Set.elem a s = s
  | otherwise = Set (a:xs)

del a (Set s) = Set (del' a s [])
  where del' _ [] acc = acc
        del' a s@(x:xs) acc
	  | a == x = acc ++ xs
	  | otherwise = del' a xs (acc ++ [x])
-}

elem a (Set (x:xs))
  | a == x = True
  | a > x = False
  | otherwise = Set.elem a (Set xs)

add a (Set q) = Set (add' a q [])
  where add' a [] _ = [a]
        add' a q@(x:xs) acc
	  | a <= x = acc ++ (a : q)
	  | otherwise = add' a xs (acc ++ [x])

del a (Set s) = Set (del' a s [])
  where del' _ [] acc = acc
        del' a s@(x:xs) acc
	  | a < x = acc ++ s	-- all larger than the wanted
	  | a == x = acc ++ xs
	  | otherwise = del' a xs (acc ++ [x])

end of list implementation -}


-- {- bit vector implementation
	
newtype Set = Set Int
instance Show Set where
  showsPrec p (Set n) str = show_set (set2list n) str

empty :: Set
is_empty :: Set -> Bool
elem :: Int -> Set -> Bool
add :: Int -> Set -> Set
del :: Int -> Set -> Set

empty = Set 0

is_empty (Set n) = n == 0

max = truncate (logBase 2 (fromIntegral (maxBound :: Int))) - 1

elem i (Set n)
  | (i >= 0) && (i <= max) = odd (n `div` 2 ^ i)
  | otherwise = error (show i ++ " is larger than max " ++ show max)

add i (Set n) =
  if Set.elem i (Set n)
     then Set n
     else Set (n + 2 ^ i)

del i (Set n) =
  if Set.elem i (Set n)
     then Set (n - 2 ^ i)
     else Set n

-- end of bit vector implementation -}


show_set :: Show a => [a] -> String -> String
show_set [] str = showChar '{' (showChar '}' str)
show_set (x:xs) str = showChar '{' (shows x (show_set' xs str))
  where show_set' [] str = showChar '}' str
        show_set' (x:xs) str = showChar ',' (shows x (show_set' xs str))

set2list :: Int -> [Int]
set2list n = set2list' n 0 []
  where set2list' 0  _ acc = acc
        set2list' n num acc =
	  if odd n
	     then set2list' (n `div` 2) (num + 1) (acc ++ [num])
	     else set2list' (n `div` 2) (num + 1) acc
