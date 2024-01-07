-- file: p21-28.hs
-- author: Jacob Xie
-- date: 2024/01/07 19:46:18 Sunday
-- brief:

import Control.Monad (replicateM)
import System.Random (Random (randomR), getStdRandom)

-- P21: Insert an element at a given position into a list
myInsertAt :: a -> [a] -> Integer -> [a]
myInsertAt x [] _ = [x]
myInsertAt x xs 1 = x : xs
myInsertAt y (x : xs) i = x : myInsertAt y xs (i - 1)

-- P22: Create a list containing all integers within a give range
myRange :: Integer -> Integer -> [Integer]
myRange i j
  | i < j = i : myRange (i + 1) j
  | otherwise = [j]

-- P23: Extract a given number of randomly selected elements from a list
myRndSelect :: [a] -> Int -> IO [a]
myRndSelect [] _ = return []
myRndSelect l n
  | n < 0 = error "N must be greater than zero."
  | otherwise = do
      pos <- replicateM n $ getStdRandom $ randomR (0, length l - 1)
      return [l !! p | p <- pos]
