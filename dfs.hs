module DFS where

import Graph
  -- FIFO
import Stack

dfs :: (Ix a, Num w) => Graph a w -> a -> [a]
  -- use 'reverse' & ':' to avoid using '++'
dfs g start = reverse (dfs' (push start empty) [])
  where
  dfs' stk acc 
    | is_empty stk = acc
    | elem (top stk) acc = dfs' (pop stk) acc
    | otherwise = dfs' (pushl (adjacent g (top stk)) (pop stk))
                       ((top stk) : acc)
