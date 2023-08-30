-- file: return1.hs
-- author: Jacob Xie
-- date: 2023/08/30 22:03:22 Wednesday
-- brief:

import Data.Char (toUpper)

isGreen :: IO Bool
isGreen = do
  putStrLn "Is green your favorite color?"
  inpStr <- getLine
  return $ (toUpper . head $ inpStr) == 'Y'
