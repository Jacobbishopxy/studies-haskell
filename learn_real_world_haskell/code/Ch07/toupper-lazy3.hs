-- file: toupper-lazy3.hs
-- author: Jacob Xie
-- date: 2023/08/29 22:22:07 Tuesday
-- brief:

import Data.Char (toUpper)

main :: IO ()
main = do
  inpStr <- readFile "input.txt"
  writeFile "output.txt" $ map toUpper inpStr
