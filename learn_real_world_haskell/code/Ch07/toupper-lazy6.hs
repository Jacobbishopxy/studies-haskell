-- file: toupper-lazy6.hs
-- author: Jacob Xie
-- date: 2023/08/29 23:09:13 Tuesday
-- brief:

import Data.Char (toUpper)

main = interact $ (++) "Your data, in uppercase, is:\n\n" . map toUpper
