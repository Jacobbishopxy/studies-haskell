-- file: MVarExample.hs
-- author: Jacob Xie
-- date: 2024/06/20 21:30:59 Thursday
-- brief:

import Control.Concurrent

communicate :: IO ()
communicate = do
  m <- newEmptyMVar
  _ <- forkIO $ do
    v <- takeMVar m
    putStrLn $ "received " ++ show v
  putStrLn "sending"
  putMVar m "wake up"
