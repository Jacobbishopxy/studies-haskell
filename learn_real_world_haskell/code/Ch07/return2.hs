-- file: return2.hs
-- author: Jacob Xie
-- date: 2023/08/30 22:36:45 Wednesday
-- brief:

import Data.Char (toUpper)

isYes :: String -> Bool
isYes inpStr = (toUpper . head $ inpStr) == 'Y'

isGreen :: IO Bool
isGreen = do
  -- putStrLn "Is green your favorite color?"
  -- inpStr <- getLine
  -- return $ isYes inpStr
  putStrLn "Is green your favorite color?"
  isYes <$> getLine
