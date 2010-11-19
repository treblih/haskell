-- dp

type List = [Int]
type Min_a = Int
type Min_b = Int
type Path_a = String
type Path_b = String

--shortest :: [Int] -> Int -> Int -> String -> String -> (Int, String)
--           list    min_a  min_b  path_a    path_b
shortest :: List -> Min_a -> Min_b -> Path_a -> Path_b -> (Int, String)
shortest [] min_a min_b path_a path_b = 
    -- end condition
    if min_a < min_b then (min_a, path_a) else (min_b, path_b)
shortest (new_a:new_b:cross:xs) min_a min_b path_a path_b =
    let a = new_a + min_a
        b = new_b + min_b
        a2b = a + cross
        b2a = b + cross
    in shortest xs (min a b2a) (min b a2b) 
        -- accelerate in reverse order
       (if a < b2a then 'A':path_a else 'C':'B':path_b)
       (if b < a2b then 'B':path_b else 'C':'A':path_a)

main :: IO()
main = do
    let list = [50,10,30,5,90,20,40,2,25,10,8,0]
        res = shortest list 0 0 "" ""
        num = fst res
        str = reverse . snd $ res
    putStrLn $ "The price is: " ++ show num
    putStrLn $ "The best path to take is: " ++ str
