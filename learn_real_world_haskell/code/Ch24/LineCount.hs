-- file: LineCount.hs
-- author: Jacob Xie
-- date: 2024/07/02 23:29:24 Tuesday
-- brief:

module Main where

import Control.Monad (forM_)
import Control.Parallel.Strategies (rdeepseq)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Int (Int64)
import LineChunks
import MapReduce
import System.Environment (getArgs)

lineCount :: [LB.ByteString] -> Int64
lineCount = mapReduce rdeepseq (LB.count '\n') rdeepseq sum

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \path -> do
    numLines <- chunkedReadWith lineCount path
    putStrLn $ path <> ": " <> show numLines
