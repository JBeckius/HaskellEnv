--reverse polish notation
-- the operator comes after the two numbers give
-- so, (4 + 3) * 10 becomes
--     4 3 + 10 *
-- Think of this like a stack!
-- Every time we see a number, put it on top of a stack.
-- when an operator is encountered, pop the top two numbers
-- of the stack, apply the operator, and push the result 
-- back onto the stack.


solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words
    where foldingFunction (x:y:ys) "*" = (y * x):ys
          foldingFunction (x:y:ys) "+" = (y + x):ys
          foldingFunction (x:y:ys) "-" = (y - x):ys
          foldingFunction (x:y:ys) "/" = (y / x):ys
          foldingFunction (x:y:ys) "^" = (y ** x):ys
          foldingFunction (x:xs) "ln" = log x:xs
          foldingFunction xs "sum" = [sum xs]
          foldingFunction xs numberString = read numberString:xs