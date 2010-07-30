module Graph (
  Graph,
  create,
  adjacent,
  nodes,
  edges_ndrct,
  edges_drct,
  edge_in,
  weight
) where

import Array

{- 
  drawbacks:
	1. not connected, each nodes generated separately
	2. no explicit val, can't be manipulated & compared directly
	   so constructing & traversing are tedious
	so use in some special manner

data Graph n w = Vertex n [((Graph n w), w)]
-}

create :: (Ix n, Num w) => Bool -> (n, n) -> [(n, n, w)] -> Graph n w
-- figure out all adjacent vertices
adjacent :: (Ix n, Num w) => Graph n w -> n -> [n]
-- how many vertices
nodes :: (Ix n, Num w) => Graph n w -> [n]
edges_ndrct :: (Ix n, Num w) => Graph n w -> [(n, n, w)]
edges_drct :: (Ix n, Num w) => Graph n w -> [(n, n, w)]
-- does 2 nodes have a edge
edge_in :: (Ix n, Num w) => Graph n w -> n -> n -> Bool
weight :: (Ix n, Num w, Show n) => Graph n w -> n -> n -> w

{- adjacent list implementation
type Graph n w = Array n [(n, w)]

create dir bnd xs =
  accumArray (\xs x -> x:xs) [] bnd ([(x1, (x2, w)) | (x1, x2, w) <- xs] ++
    if dir
       then []
       else [(x2, (x1, w)) | (x1, x2, w) <- xs, x1 /= x2])

adjacent g x = map fst (g ! x)

nodes g = indices g

edge_in g x y = elem x $ adjacent g y

weight g x y
  | edge_in g x y = head [w | (n, w) <- g ! x, n == y]
  | otherwise = error (show x ++ " and " ++ show y ++ " are not adjacent")

edges_ndrct g = [(x1, x2, w) | x1 <- nodes g, (x2, w) <- g ! x1, x1 < x2]

edges_drct g = [(x1, x2, w) | x1 <- nodes g, (x2, w) <- g ! x1]
-}

-- adjacent matrix implementation
