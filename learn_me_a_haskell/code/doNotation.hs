-- file: doNotation.hs
-- author: Jacob Xie
-- date: 2023/08/06 20:33:48 Sunday
-- brief:

import Control.Monad (guard)

foo :: Maybe String
-- foo = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
foo = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)

marySue :: Maybe Bool
marySue = do
  x <- Just 9
  Just (x > 8)

justH :: Maybe Char
justH = do
  (x : _) <- Just "hello"
  return x

listOfTuples :: [(Int, Char)]
listOfTuples = do
  n <- [1, 2]
  ch <- ['a', 'b']
  return (n, ch)

sevensOnly :: [Int]
sevensOnly = do
  x <- [1 .. 50]
  guard ('7' `elem` show x)
  return x

main = do
  putStrLn $ "list of tuples: " ++ show listOfTuples

  putStrLn $ "sevens only: " ++ show sevensOnly
