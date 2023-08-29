-- file: toupper-lazy5.hs
-- author: Jacob Xie
-- date: 2023/08/29 23:06:47 Tuesday
-- brief:

import Data.Char (toUpper)

main = interact $ map toUpper . (++) "Your data, in uppercase, is:\n\n"
