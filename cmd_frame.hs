import System.Environment (getArgs)

interact_with :: (String -> String) -> FilePath -> FilePath -> IO ()
interact_with f infile outfile = do
    input <- readFile infile
    writeFile outfile (f input)

main = getArgs >>= (\arg -> case arg of 
            [infile, outfile] -> interact_with fst_word_each_line infile outfile
            _ -> putStrLn "error: 2 args, first is INPUT the other is OUTPUT")

fst_word_each_line :: String -> String
fst_word_each_line = unlines . map f . lines
    where
    f [] = []
    f xs = head . words $ xs
