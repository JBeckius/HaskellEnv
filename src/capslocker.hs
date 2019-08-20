import Control.Monad
import Data.Char

-- main = forever $ do
--     l <- getLine
--     putStrLn $ map toUpper l

-- Replace 'forever' with the helper function 'getContents'

-- main = do
--     contents <- getContents
--     putStr $ map toUpper contents

-- Only short lines now!

-- main = do
--     contents <- getContents
--     putStr $ shortLinesOnly contents

-- shortLinesOnly :: String -> String
-- shortLinesOnly = unlines . filter (\line -> length line < 10) . lines

-- Even less code thanks to the 'interact' helper function
-- It takes a function of type String -> String and returns
-- an I/O action that will take some input, run that function on it
-- and then print out the function's result.

-- main = interact shortLinesOnly

-- shortLinesOnly :: String -> String
-- shortLinesOnly = unlines . filter (\a -> length a < 10) . lines

-- Now let's tell the user if each line is a palindrome

main = interact respondPalindromes

respondPalindromes :: String -> String
respondPalindromes = unlines . map (\xs -> if isPal xs then "palindrome" else "not a palindrome") . lines

isPal :: String -> Bool
isPal xs = xs == reverse xs