module Queue (
  Queue,
  empty,
  is_empty,
  add,
  rem
) where

data Queue a = Queue [a] [a]

empty :: Queue a
empty = Queue [] []

is_empty :: Queue a -> Bool
is_empty (Queue [] []) = True
is_empty (Queue _ _) = False

add :: Queue a -> a -> Queue a
add (Queue ls rs) x = Queue ls (x:rs)

rem :: Queue a -> (a, Queue a)
rem (Queue (l:ls) rs) = (l, Queue ls rs)
rem (Queue [] rs) = rem (Queue (reverse rs []))
rem (Queue _ _) = error "empty queue"
