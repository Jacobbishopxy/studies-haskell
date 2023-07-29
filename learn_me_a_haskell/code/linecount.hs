-- file: linecount.hs
-- author: Jacob Xie
-- date: 2023/07/29 11:29:37 Saturday
-- brief:

-- import System.Environment
-- import System.IO

-- main = do
--   (fileName : _) <- getArgs
--   contents <- readFile fileName
--   let l = show . length . lines
--   putStrLn $ "This file has " ++ contents ++ " lines!"

import System.Directory
import System.Environment
import System.IO

main = do
  (fileName : _) <- getArgs
  fileExists <- doesFileExist fileName
  if fileExists
    then do
      contents <- readFile fileName
      let l = show . length . lines
      putStrLn $ "The file has " ++ l contents ++ " lines!"
    else do
      putStrLn "The file doesn't exists!"
