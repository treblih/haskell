module DivideConquer (
  divide_conquer
) where

divide_conquer :: p ->
		  (p -> [p]) ->  	-- divide
		  (p -> Bool) ->	-- is indivisible?
		  (p -> s) ->		-- solve
		  (p -> [s] -> s) ->	-- combine, p is essential
		  s
divide_conquer prob divide indivisible solve combine = dc' prob
  where
  dc' prob
    | indivisible prob = solve prob
    | otherwise = combine prob (map dc' (divide prob))

merge_sort' xs = divide_conquer xs divide indivisible id combine
    where indivisible xs = length xs <= 1
          divide xs = let n = length xs `div` 2
                      in [take n xs , drop n xs]
          combine _ [l1,l2] = merge l1 l2

merge_sort :: Ord a => [a] -> [a]
merge_sort xs = divide_conquer xs divide indivisible solve combine
  where
  divide xs = let n = length xs `div` 2
              in [take n xs , drop n xs]
  indivisible xs = length xs <= 1
  solve [] = []
  solve xs@[x] = xs
  solve xs@[x1, x2]
    | x1 <= x2 = xs
    | otherwise = [x2, x1]
  combine _ [xs1, xs2] = merge xs1 xs2

merge :: Ord a => [a] -> [a] -> [a]
merge x [] = x
merge [] y = y
merge xl@(x:xs) yl@(y:ys)
  | x <= y = x : merge xs yl
  | otherwise = y : merge ys xl
