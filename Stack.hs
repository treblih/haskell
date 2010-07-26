module Stack(
  Stack,
  empty,
  is_empty,
  push,
  pop,
  top
) where

{-
data Stack a = Empty | Stack a (Stack a)
instance Show a => Show (Stack a) where
  showsPrec _ Empty str = showChar '-' str
  showsPrec _ (Stack x xs) str = shows x (showChar '|' (shows xs str))
-}

newtype Stack a = Stack [a]
instance Show a => Show (Stack a) where
  showsPrec _ (Stack []) str = showChar '-' str
  showsPrec _ (Stack (x:xs)) str = shows x (showChar '|' (shows (Stack xs) str))

empty :: Stack a
is_empty :: Stack a -> Bool
push :: a -> Stack a -> Stack a
pop :: Stack a -> Stack a
top :: Stack a -> a

{-
empty = Empty

is_empty Empty = True
is_empty _ = False

push x xs = Stack x xs

pop Empty = error "empty stack"
pop (Stack _ xs) = xs

top Empty = error "empty stack"
top (Stack x _) = x
-}

empty = Stack []

is_empty (Stack []) = True
is_empty _ = False

push x (Stack xs) = Stack (x:xs)

pop (Stack []) = error "empty stack"
pop (Stack (_:xs)) = Stack xs

top (Stack []) = error "empty stack"
top (Stack (x:_)) = x
