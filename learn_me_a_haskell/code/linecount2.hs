-- file: linecount2.hs
-- author: Jacob Xie
-- date: 2023/07/29 12:16:27 Saturday
-- brief:

import Control.Exception
import System.Environment
import System.IO.Error

main = toTry `catch` handler

toTry :: IO ()
toTry = do
  (fileName : _) <- getArgs
  contents <- readFile fileName
  let l = show . length . lines
  putStrLn $ "The file has " ++ l contents ++ " lines!"

-- version#1
-- handler :: IOError -> IO ()
-- handler e = putStrLn "Whoops, had some trouble!"

-- version#2
-- handler :: IOError -> IO ()
-- handler e
--   | isDoesNotExistError e = putStrLn "The file doesn't exist!"
--   | otherwise = ioError e

-- version#3
handler :: IOError -> IO ()
handler e
  | isDoesNotExistError e = putStrLn "The file doesn't exist!"
  | isFullError e = freeSomeSpace
  | isIllegalOperation e = notifyCops
  | otherwise = ioError e

-- mock
freeSomeSpace :: IO ()
freeSomeSpace =
  putStrLn "freeSomeSpace"

-- mock
notifyCops :: IO ()
notifyCops =
  putStrLn "notifyCops"
