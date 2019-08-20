import System.IO

main = do
    filename <- getLine
    handle <- openFile filename ReadMode
    contents <- hGetContents handle
    putStr contents
    hClose handle