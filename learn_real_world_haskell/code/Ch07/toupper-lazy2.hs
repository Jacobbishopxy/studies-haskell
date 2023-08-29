-- file: toupper-lazy2.hs
-- author: Jacob Xie
-- date: 2023/08/29 21:15:37 Tuesday
-- brief:

import Data.Char (toUpper)
import System.IO

main :: IO ()
main = do
  inh <- openFile "input.txt" ReadMode
  outh <- openFile "output.txt" WriteMode
  inpStr <- hGetContents inh
  hPutStr outh (map toUpper inpStr)
  hClose inh
  hClose outh
