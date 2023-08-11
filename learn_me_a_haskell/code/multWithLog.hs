-- file: multWithLog.hs
-- author: Jacob Xie
-- date: 2023/08/10 22:54:56 Thursday
-- brief:

import Control.Monad.Trans.Writer

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["Gonna multiply these two"]
  return (a * b)

writerTest :: Int -> String -> String
writerTest x y = show (runWriter (writer (x, y)))

main :: IO ()
main = do
  putStrLn $ "writer test: " ++ writerTest 3 "haha"

  putStrLn $ "whatever" ++ show multWithLog
