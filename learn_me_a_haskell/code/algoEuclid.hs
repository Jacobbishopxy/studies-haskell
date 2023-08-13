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

gcdReverse :: Int -> Int -> Writer [String] Int
gcdReverse a b
  | b == 0 = do
      tell ["Finished with " ++ show a]
      return a
  | otherwise = do
      result <- gcdReverse b (a `mod` b)
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      return result

main :: IO ()
main = do
  let res1 = runWriter (gcd' 8 3)

  putStrLn $ "gcd result [1]: " ++ show (fst res1)
  mapM_ putStrLn $ snd res1

  let res2 = runWriter (gcdReverse 8 3)
  putStrLn $ "gcdReverse result [2]: " ++ show (fst res2)
  mapM_ putStrLn $ snd res2
