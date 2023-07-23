-- file: girlfriend.hs
-- author: Jacob Xie
-- date: 2023/07/23 09:17:46 Sunday
-- brief:

import System.IO

-- Version#1
-- main = do
--   handle <- openFile "girlfriend.txt" ReadMode
--   contents <- hGetContents handle
--   putStr contents
--   hClose handle

-- Version#2, using `withFile`
-- main = do
--   withFile
--     "girlfriend.txt"
--     ReadMode
--     ( \handle -> do
--         contents <- hGetContents handle
--         putStr contents
--     )

-- Version#3, using `withFile'`
main = do
  withFile'
    "girlfriend.txt"
    ReadMode
    ( \handle -> do
        contents <- hGetContents handle
        putStr contents
    )

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f = do
  handle <- openFile path mode
  result <- f handle
  hClose handle
  return result
