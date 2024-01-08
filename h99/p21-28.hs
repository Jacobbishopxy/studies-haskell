-- file: p21-28.hs
-- author: Jacob Xie
-- date: 2024/01/07 19:46:18 Sunday
-- brief:

import Control.Monad (replicateM)
import Control.Monad.State
import Data.List
import Data.Set qualified as Set
import System.Random

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
myRndSelect xs n
  | n < 0 = error "N must be greater than zero."
  | otherwise = do
      pos <- replicateM n $ getStdRandom $ randomR (0, length xs - 1)
      return [xs !! p | p <- pos]

myRndSelect' :: [a] -> Int -> IO [a]
myRndSelect' xs n
  | n < 0 = error "N must be greater than zero."
  | otherwise = replicateM n rand
  where
    rand = do
      r <- randomRIO (0, length xs - 1)
      return (xs !! r)

-- P24: Lotto: Draw N different random numbers from the set 1..M
myDiffSelect :: Int -> Int -> IO [Int]
myDiffSelect 0 _ = return []
myDiffSelect i k
  | i > k = error "selected number is greater than the size of set 1..M"
  | otherwise = f i [1 .. k]
  where
    f :: Int -> [Int] -> IO [Int]
    f i xs
      | i == 0 = return []
      | otherwise = do
          r <- getStdRandom $ randomR (0, length xs - 1)
          let x = xs !! r
          l <- f (i - 1) (delete x xs)
          return (x : l)

myDiffSelect' :: (Eq a) => Int -> [a] -> [a]
myDiffSelect' 0 _ = []
myDiffSelect' i xs = evalState (replicateM i f) xs
  where
    f :: (Eq a) => State [a] a
    f = do
      remainder <- get
      let size = length remainder
          gen = mkStdGen size
          (index, newGen) = randomR (0, size - 1) gen
          num = remainder !! index
      put $ delete num remainder
      return num

-- P25: Generate a random permutation of the elements of a list
myRndPermu :: (Eq a) => [a] -> [a]
myRndPermu [] = []
myRndPermu xs = myDiffSelect' (length xs) xs
