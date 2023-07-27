-- file: readWriteAppend.hs
-- author: Jacob Xie
-- date: 2023/07/23 22:29:13 Sunday
-- brief:

import Data.Char
import System.IO

-- readFile
readGirlfriend :: IO String
readGirlfriend = do
  readFile "girlfriend.txt"

-- writeFile
writeGirlfriend :: IO ()
writeGirlfriend = do
  contents <- readFile "girlfriend.txt"
  writeFile "girlfriendcaps.txt" (map toUpper contents)

-- appendFile
appendTodo :: IO ()
appendTodo = do
  todoItem <- getLine
  appendFile "todo.txt" $ todoItem ++ "\n"
