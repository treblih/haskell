import Char

main = (writeFile "out.txt" . map toUpper) <- readFile "tmp.hs"
