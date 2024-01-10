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

-- P26: Generate combinations of K distinct objects chosen from the N elements of a list
-- combinations 5 "abcdef"
-- ["abcde","abcdf","abcef","abdef","acdef","bcdef"]
myCombinations :: Int -> [a] -> [[a]]
myCombinations _ [] = [[]]
myCombinations 0 _ = [[]]
-- Get all combinations that start with x, recursively choosing (k-1) from the
-- remaining xs. After exhausting all the possibilities starting with x, if there
-- are at least k elements in the remaining xs, recursively get combinations of k
-- from the remaining xs.
myCombinations i (x : xs) = x_start ++ others
  where
    x_start = [x : rest | rest <- myCombinations (i - 1) xs]
    others
      | i <= length xs = myCombinations i xs
      | otherwise = []

-- std fn: `tails "abc"` get `["abc","bc","c",""]`
myCombinations' :: Int -> [a] -> [[a]]
myCombinations' 0 _ = [[]]
myCombinations' i xs = [y : ys | y : xs' <- tails xs, ys <- myCombinations' (i - 1) xs']

-- alternate syntax by using 'do' block
myCombinations'' :: Int -> [a] -> [[a]]
myCombinations'' 0 _ = return []
myCombinations'' i xs = do
  y : xs' <- tails xs
  ys <- myCombinations'' (i - 1) xs'
  return (y : ys)

-- P27: Group the elements of a set into disjoint subsets
-- group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
-- group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
myGroup :: [Int] -> [a] -> [[[a]]]
myGroup [] _ = [[]]
myGroup (i : is) xs = [y : ys | (y, zs) <- combination i xs, ys <- myGroup is zs]
  where
    combination :: Int -> [a] -> [([a], [a])]
    combination 0 xs = [([], xs)]
    combination _ [] = []
    combination i (x : xs) = ts ++ ds
      where
        ts = [(x : ys, zs) | (ys, zs) <- combination (i - 1) xs]
        ds = [(ys, x : zs) | (ys, zs) <- combination i xs]

-- P28: Sorting a list of lists according to length of sublists
myLSort :: [[a]] -> [[a]]
myLSort [] = []
myLSort (x : xs) = f x $ myLSort xs
  where
    f :: [a] -> [[a]] -> [[a]]
    f x [] = [x]
    f x (x' : xs)
      | length x <= length x' = x : x' : xs
      | otherwise = x' : f x xs

-- TODO
myLFSort :: [[a]] -> [[a]]
myLFSort [] = []
myLFSort xs = undefined
  where
    f :: [[a]] -> [(Int, Int)]
    f [] = []
