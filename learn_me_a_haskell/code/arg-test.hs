-- file: arg-test.hs
-- author: Jacob Xie
-- date: 2023/07/25 22:12:15 Tuesday
-- brief:

import Data.List
import System.Environment

main = do
  args <- getArgs
  progName <- getProgName
  putStrLn "The arguments are:"
  mapM_ putStrLn args
  putStrLn "The program name is:"
  putStrLn progName
