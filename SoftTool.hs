import IO
import System
import Data.Char
import Data.List


{- ================ 1. Bacis Character IO ================ -}
getc :: IO (Maybe Char)
getc = isEOF >>= (\eof -> 
  if eof
    then return Nothing
    else getChar >>= return . Just)

putc :: Char -> IO ()
putc = putChar

copy :: IO ()
copy = getc >>= (\c -> 
  case c of
    Nothing -> return ()
    Just c -> putc c >>
              copy)
copy' :: IO ()
copy' = interact id

char_cnt :: IO ()
char_cnt = cnt 0 >>= return . show >>= putStrLn
  where cnt n = getc >>= (\c ->
		  case c of
		    Nothing -> return n
		    Just c -> cnt $! n + 1)
char_cnt' :: IO ()
char_cnt' = interact $ \input -> (show . length) input ++ "\n"

word_cnt :: IO ()
word_cnt = cnt False 0 >>= return . show >>= putStrLn
  where cnt inword n = getc >>= (\c ->
		  	 case c of
			   Nothing -> return n
			   Just c -> char_handler c inword n)
  	char_handler c _ n			-- all Space cases here
	  | isSpace c = cnt False n
	char_handler _ False n = cnt True $! n + 1  -- a new word start
	char_handler _ True n  = cnt True n  -- inside a word
word_cnt' :: IO ()
word_cnt' = interact $ \input -> (show . length . words) input ++ "\n"

line_cnt :: IO ()
line_cnt = cnt 1 >>= return . show >>= putStrLn  -- cnt 1, not 0
  where cnt n = getc >>= (\c ->
		  case c of
		    Nothing -> return n
		    Just '\n' -> cnt $! n + 1  -- a new line
		    Just _ -> cnt $! n)
line_cnt' :: IO ()
line_cnt' = interact $ \input -> (show . length . lines) input ++ "\n"

wc :: IO ()
wc = interact $ \input -> (show_cwl . cnt (0, 0, 0) False) input
  where cnt (c, w, l) _ [] = (c, w, l)
        cnt (c, w, l) inword (x:xs)
	  | isLn x = (seq . force) (c, w, l) $ cnt (c + 1, w, l + 1) False xs
	  | isSpace x = (seq . force) (c, w, l) $ cnt (c + 1, w, l) False xs  -- Space cases
	cnt (c, w, l) True (_:xs)  = (seq . force) (c, w, l) $ cnt (c + 1, w, l) True xs
	cnt (c, w, l) False (_:xs) = (seq . force) (c, w, l) $ cnt (c + 1, w + 1, l) True xs

	show_cwl (c, w, l) = "Chars: " ++ show c ++ " Words: " ++ show w ++ 
	                     " Lines: " ++ show l ++ "\n"

	force (c, w, l) = seq (seq c w) l

  	isLn c = c == '\n'

tab2spc :: IO ()
tab2spc = interact $ unlines . map (tab2spc' ts) . lines
  where ts = cycle [4,3,2,1]

  	tab2spc' _ [] = []
	-- tab2spc' _ ('\n':text) = '\n' : tab2spc' ts text
	-- process 1 line by 1, no need this
	tab2spc' (n:ns) ('\t':text) = replicate n ' ' ++ tab2spc' (drop (n - 1) ns) text
	tab2spc' (_:ns) (char:text) = char : tab2spc' ns text


{- ================ 2. Bacis Processing ================ -}
spc2tab :: IO ()
spc2tab = interact $ unlines . map (spc2tab' ts) . lines
  where ts = cycle [4,3,2,1]

  	spc2tab' _ [] = []
	-- spc2tab' _ ('\n':text) = '\n' : spc2tab' ts text
	-- process 1 line by 1, no need this
	spc2tab' (n:ns) (' ':text)
	  | take (n - 1) text == replicate (n - 1) ' ' =
	      '\t' : spc2tab' (drop (n - 1) ns) (drop (n - 1) text)
	  | otherwise = ' ' : spc2tab' ns text
	spc2tab' (_:ns) (char:text) = char : spc2tab' ns text

echo_args = getArgs >>= mapM_ putc . concat . intersperse " "
	
main = char_cnt
