module Soring (
  merge,
  quick_sort,
  merge_sort,
  bucket_sort,
  topo_sort,
  topo_sort_num
) where

import Array
import Graph.Graph

merge :: Ord a => [a] -> [a] -> [a]
merge x [] = x
merge [] y = y
merge xl@(x:xs) yl@(y:ys)
  | x <= y = x : merge xs yl
  | otherwise = y : merge ys xl

quick_sort :: Ord a => [a] -> [a]
quick_sort l = quick_sort' l []
  where
  quick_sort' [] acc = acc
  quick_sort' (x:xs) acc = quick_sort' [sml | sml <- xs, sml <= x]
	                     (x : 
		             (quick_sort' [lrg | lrg <- xs, lrg > x] acc))

merge_sort :: Ord a => [a] -> [a]
merge_sort l = ll2l (split l)
  where
  split :: Ord a => [a] -> [[a]]
  split [] = []
  split [x] = [[x]]
  split (x:y:s)
    | x <= y = [x, y] : split s
    | otherwise = [y, x] : split s

  ll2l :: Ord a => [[a]] -> [a]
  ll2l [x] = x
  ll2l l = ll2l (merge_pair l)

  merge_pair :: Ord a => [[a]] -> [[a]]
  merge_pair [] = []
  merge_pair l@[x] = l
  merge_pair (x:y:s) = merge x y : merge_pair s



-- type Bucket x n = Array x [(x, n)]

bucket_sort :: (Ix i, Num i) => (i, i) -> [i] -> [i]
bucket_sort bnd xs =
  generate (fst bnd) (elems buckets) []
    where
    buckets = accumArray (+) 0 bnd [(n, 1) | n <- xs]

    generate :: Num a => a -> [Int] -> [a] -> [a]
    generate _ [] acc = acc -- finish condition
    generate x (n:ns) acc = generate (x + 1) ns (acc ++ replicate n x)

topo_sort :: (Ix a, Num w) => Graph a w -> [a]
topo_sort g = tsort [n | n <- nodes g, (indegree g n == 0)] []
  where
  tsort [] acc = acc
  tsort (x:xs) acc
    | elem x acc = tsort xs acc
    | otherwise = tsort xs (x:(tsort (shoot g x) acc))

  -- just for digit DAG
topo_sort_num :: (Ix a, Num a, Num w) => Graph a w -> [a]
topo_sort_num g = tsort (indegree_0 g) []
  where
  tsort [] acc = acc
  tsort (x:xs) acc
    | elem x acc = tsort xs acc
    | otherwise = tsort xs (x:(tsort (shoot g x) acc))
