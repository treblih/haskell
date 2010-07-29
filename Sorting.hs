module Soring (
  quick_sort,
  merge_sort
) where

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

  merge :: Ord a => [a] -> [a] -> [a]
  merge x [] = x
  merge [] y = y
  merge xl@(x:xs) yl@(y:ys)
    | x <= y = x : merge xs yl
    | otherwise = y : merge ys xl
