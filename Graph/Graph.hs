module Graph (
  Array(..),
  Ix(..),
  Graph,
  create,
  adjacent,
  shoot,
  nodes,
  weight,
  edge_in,
  edges_ndrct,
  edges_drct,
  indegree,
  indegree_0
) where

import Array

{- pointer implementation
  drawbacks:
	1. not connected, each nodes generated separately
	2. no explicit val, can't be manipulated & compared directly
	   so constructing & traversing are tedious
	so use in some special manner

data Graph n w = Vertex n [((Graph n w), w)]
-}

create :: (Ix n, Num w) => Bool -> (n, n) -> [(n, n, w)] -> Graph n w
  -- all adjacent vertices in undirected graph
adjacent :: (Ix n, Num w) => Graph n w -> n -> [n]
  -- all shot by the one in directed graph
shoot :: (Ix n, Num w) => Graph n w -> n -> [n]
  -- how many vertices
nodes :: (Ix n, Num w) => Graph n w -> [n]
weight :: (Ix n, Num w, Show n) => Graph n w -> n -> n -> w
  -- does 2 nodes have a edge
edge_in :: (Ix n, Num w) => Graph n w -> n -> n -> Bool
edges_ndrct :: (Ix n, Num w) => Graph n w -> [(n, n, w)]
edges_drct :: (Ix n, Num w) => Graph n w -> [(n, n, w)]
indegree :: (Ix n, Num w) => Graph n w -> n -> Int
indegree_0 :: (Ix n, Num n, Num w) => Graph n w -> [n]

{- adjacent list implementation
type Graph n w = Array n [(n, w)]

create dir bnd xs =
  accumArray (\xs x -> x:xs) [] bnd ([(x1, (x2, w)) | (x1, x2, w) <- xs] ++
    if dir
       then []
       else [(x2, (x1, w)) | (x1, x2, w) <- xs, x1 /= x2])

adjacent g x = map fst (g ! x)

shoot = adjacent

nodes g = indices g

weight g x y
  | edge_in g x y = head [w | (n, w) <- g ! x, n == y]
  | otherwise = error (show x ++ " and " ++ show y ++ " are not adjacent")

edge_in g x y = elem x $ adjacent g y

edges_ndrct g = [(x1, x2, w) | x1 <- nodes g, (x2, w) <- g ! x1, x1 < x2]

edges_drct g = [(x1, x2, w) | x1 <- nodes g, (x2, w) <- g ! x1]
-}

  -- adjacent matrix implementation
type Graph n w = Array (n, n) (Maybe w)

create dir bnd@(l, u) xs =
  matrix_empty // ([((x, y), Just w) | (x, y, w) <- xs] ++
                   if dir
		      then []
		      else [((y, x), Just w) | (x, y, w) <- xs, x /= y])
    where
    matrix_empty = array ((l, l), (u, u)) 
	           [((x1, x2), Nothing) | x1 <- range bnd, x2 <- range bnd]

adjacent g x = [y | y <- nodes g, g ! (x, y) /= Nothing]

shoot = adjacent

nodes g = range (l, u) where ((l, _), (u, _)) = bounds g

weight g x y = w where Just w = g ! (x, y)

edge_in g x y = g ! (x, y) /= Nothing

edges_ndrct g = [(x, y, unwrap $ g ! (x, y)) |
                  x <- nodes g, y <- nodes g, edge_in g x y]

edges_drct g = [(x, y, unwrap $ g ! (x, y)) | 
                 x <- nodes g, y <- range (x, u), edge_in g x y]
  where (_, (u, _)) = bounds g

unwrap :: Maybe a -> a
unwrap (Just a) = a

indegree g n  = length [y | x <- nodes g, y <- adjacent g x, (n == y)]

  -- (Num n) the index need to be added
  -- just for number node, not Enum ones
  -- indegree_0 :: (Ix n, Num n, Num w) => Graph n w -> [n]
indegree_0 g = indegree_0' g l l []
  where
  ((l, _), (u, _)) = bounds g

  indegree_0' g x y acc
    | x == u && y == u =
        if g ! (u, u) /= Nothing
	   then acc
	   else u:acc
    | y == u && g ! (x, u) /= Nothing = acc
    | x == u && g ! (u, y) == Nothing = indegree_0' g l (y + 1) (y:acc)
    | g ! (x, y) /= Nothing = indegree_0' g l (y + 1) acc
    | g ! (x, y) == Nothing = indegree_0' g (x + 1) y acc
