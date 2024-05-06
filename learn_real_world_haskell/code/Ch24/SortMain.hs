-- file: SortMain.hs
-- author: Jacob Xie
-- date: 2024/04/30 09:12:40 Tuesday
-- brief:

module Main where

import Data.Time.Clock
import qualified Sorting as S
import System.Environment (getArgs)
import System.Random

testFunction :: Int -> [Int] -> [Int]
testFunction n
  | n == 1 = S.sort
  -- \| n == 2 = S.seqSort
  | n == 3 = S.parSort
  -- \| n == 4 = S.parSort2
  | otherwise = error "choose n from 1 to 4"

randomInts :: Int -> StdGen -> [Int]
randomInts k g =
  let result = take k (randoms g)
   in S.force result `seq` result

-- test:
-- cabal run sort-main 3 500000
main :: IO ()
main = do
  args <- getArgs

  case args of
    (sortMethod : count : _) -> do
      input <- randomInts (read count) `fmap` getStdGen
      putStrLn $ "We have " ++ show (length input) ++ " elements to sort."
      start <- getCurrentTime
      let sorted = testFunction (read sortMethod) input
      putStrLn $ "Sorted all " ++ show (length sorted) ++ " elements."
      end <- getCurrentTime
      putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
    _ -> error "SortMain <sortMethod:Int> <count:Int>"
