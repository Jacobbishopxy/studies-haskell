-- file: algoEuclid.hs
-- author: Jacob Xie
-- date: 2023/08/11 23:42:18 Friday
-- brief:

import Control.Monad.Trans.Writer

-- gcd' :: Int -> Int -> Int
-- gcd' a b
--   | b == 0 = a
--   | otherwise = gcd' b (a `mod` b)

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = do
      tell ["Finished with " ++ show a]
      return a
  | otherwise = do
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      gcd' b (a `mod` b)

main :: IO ()
main = do
  let res = runWriter (gcd' 8 3)

  putStrLn $ "test result: " ++ show (fst res)
  mapM_ putStrLn $ snd res
