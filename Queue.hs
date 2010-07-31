module Queue (
  Queue,
  empty,
  is_empty,
  enque,
  enquel,
  deque,
  front
) where

newtype Queue a = Queue ([a], [a]) deriving Show
-- data Queue a = Queue [a], [a]

empty :: Queue a
is_empty :: Queue a -> Bool
enque :: a -> Queue a -> Queue a
enquel :: [a] -> Queue a -> Queue a
deque :: Queue a -> Queue a
front :: Queue a -> a


empty = Queue ([], [])

is_empty (Queue ([], [])) = True
is_empty (Queue (_, _)) = False

  -- attach to the ts
enque x (Queue (hs, ts)) = Queue (hs, (x:ts))

enquel list q = foldr enque q list

  -- remove from the hs
deque (Queue ([], [])) = error "empty queue"
deque (Queue ([], ts)) = deque (Queue (reverse ts, []))
deque (Queue ((_:hs), ts)) = Queue (hs, ts)

front (Queue ([], [])) = error "empty queue"
front (Queue ([], ts)) = front (Queue (reverse ts, []))
-- front (Queue ([], ts)) = last ts
front (Queue ((h:_), _)) = h
