
rpn :: (Num a, Read a) => String -> a
rpn = head . foldl f [] . words
    where
    f (x:y:xs) "*" = x * y : xs
    -- f (x:y:xs) "/" = x / y : xs
    f (x:y:xs) "+" = x + y : xs
    f (x:y:xs) "-" = x - y : xs
    f xs digits = read digits : xs
