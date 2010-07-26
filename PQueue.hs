module PQueue(
  PQueue,
  empty,
  is_empty,
  add,
  del,
  front
) where

import Prelude hiding (del)

newtype PQueue a = PQueue [a]
instance Show a => Show (PQueue a) where
  showsPrec p (PQueue a) str = showString "Q: " (showList a str)

empty :: PQueue a
is_empty :: PQueue a -> Bool
add :: Ord a => a -> PQueue a -> PQueue a
del :: PQueue a -> PQueue a
front :: PQueue a -> a

empty = PQueue []

is_empty (PQueue []) = True
is_empty _ = False

add a (PQueue q) = PQueue (add' a q [])
  where add' a [] _ = [a]
        add' a q@(x:xs) acc
	  | a <= x = acc ++ (a : q)
	  | otherwise = add' a xs (acc ++ [x])

del (PQueue []) = error "empty pqueue"
del (PQueue (_:xs)) = PQueue xs

front (PQueue []) = error "empty pqueue"
front (PQueue (x:_)) = x
