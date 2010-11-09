module BFS where

import Graph
  -- FILO
import Queue

bfs :: (Ix a, Num w) => Graph a w -> a -> [a]
  -- use 'reverse' & ':' to avoid using '++'
bfs g start = reverse (bfs' (enque start empty) [])
  where
  bfs' que acc
    | is_empty que = acc
    | elem (front que) acc = bfs' (deque que) acc
    | otherwise = bfs' (enquel (adjacent g (front que)) (deque que))
                       ((front que):acc)
