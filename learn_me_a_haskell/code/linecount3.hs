-- file: linecount3.hs
-- author: Jacob Xie
-- date: 2023/07/29 13:11:21 Saturday
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

handler :: IOError -> IO ()
handler e
  | isDoesNotExistError e =
      case ioeGetFileName e of
        Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path
        Nothing -> putStrLn "Whoops! File does not exist at unknown location!"
  | otherwise = ioError e
