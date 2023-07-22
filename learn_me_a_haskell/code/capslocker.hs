-- file: capslocker.hs
-- author: Jacob Xie
-- date: 2023/07/21 23:37:00 Friday
-- brief:

-- import Control.Monad
-- import Data.Char

-- main = forever $ do
--   putStr "Give me some input: "
--   l <- getLine
--   putStrLn $ map toUpper l

import Data.Char

main = do
  contents <- getContents
  putStr $ map toUpper contents
