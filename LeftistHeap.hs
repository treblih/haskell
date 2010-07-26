module LeftistHeap (
  LeftistHeap,
  empty,
  is_empty,
  node_val,
  pl,
  merge,
  add,
  del,
  list2leftist
) where

data LeftistHeap a =
     Empty |
     LeftistHeap a Int (LeftistHeap a) (LeftistHeap a)
instance Show a => Show (LeftistHeap a) where
  showsPrec _ h str = showList (leftist2list h) str

empty :: LeftistHeap a
is_empty :: LeftistHeap a -> Bool
node_val :: LeftistHeap a -> a
pl :: LeftistHeap a -> Int
merge :: Ord a => LeftistHeap a -> LeftistHeap a -> LeftistHeap a
add :: Ord a => a -> LeftistHeap a -> LeftistHeap a
del :: Ord a => LeftistHeap a -> LeftistHeap a

empty = Empty

is_empty Empty = True
is_empty _ = False

node_val Empty = error "empty leftist heap"
node_val (LeftistHeap x _ _ _) = x

pl Empty = 0
pl (LeftistHeap _ x _ _) = x

-- merge swap the children in terms of val
-- while merge' in terms of pl (path length)
merge h Empty = h
merge Empty h = h
merge h1 h2
  | node_val h1 <= node_val h2 = merge' h1 h2  -- the smaller will be the main
  | otherwise = merge' h2 h1
  where merge' main@(LeftistHeap x1 pl1 ls1 rs1) sub@(LeftistHeap _ pl2 _ _)
          | pl1 >= pl2 = LeftistHeap x1 (pl2 + 1) ls1 (merge rs1 sub)
	  | otherwise = LeftistHeap x1 (pl1 + 1) (merge rs1 sub) ls1

add a h = merge (LeftistHeap a 0 Empty Empty) h

del Empty = error "empty leftist heap"
del (LeftistHeap _ _ ls rs) = merge ls rs

-- preorder traverse
leftist2list :: LeftistHeap a -> [a]
leftist2list Empty = []
leftist2list (LeftistHeap a _ ls rs) = [a] ++ leftist2list ls ++ leftist2list rs

list2leftist :: Ord a => [a] -> LeftistHeap a
list2leftist l = list2leftist' l Empty
  where list2leftist' [] acc = acc
        list2leftist' (x:xs) acc = list2leftist' xs (add x acc)
