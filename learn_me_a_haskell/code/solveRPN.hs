-- file: solveRPN.hs
-- author: Jacob Xie
-- date: 2023/07/29 14:32:39 Saturday
-- brief:

-- solveRPN :: (Num a, Read a) => String -> a
-- solveRPN = head . foldl foldingFunction [] . words
--   where
--     foldingFunction (x : y : ys) "*" = (x * y) : ys
--     foldingFunction (x : y : ys) "+" = (x + y) : ys
--     foldingFunction (x : y : ys) "-" = (y - x) : ys
--     foldingFunction xs numberString = read numberString : xs

solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
  where
    foldingFunction (x : y : ys) "*" = (x * y) : ys
    foldingFunction (x : y : ys) "+" = (x + y) : ys
    foldingFunction (x : y : ys) "-" = (y - x) : ys
    foldingFunction (x : y : ys) "/" = (y / x) : ys
    foldingFunction (x : y : ys) "^" = (y ** x) : ys
    foldingFunction (x : xs) "ln" = log x : xs
    foldingFunction xs "sum" = [sum xs]
    foldingFunction xs numberString = read numberString : xs
