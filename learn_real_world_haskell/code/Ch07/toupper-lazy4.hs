-- file: toupper-lazy4.hs
-- author: Jacob Xie
-- date: 2023/08/29 22:50:31 Tuesday
-- brief:

import Data.Char (toUpper)

main :: IO ()
main = interact $ map toUpper
