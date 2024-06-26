-- file: Expensive.hs
-- author: Jacob Xie
-- date: 2024/06/26 20:48:18 Wednesday
-- brief:

import Control.Concurrent

-- Don't do this ever! The computation is actually happened in the main thread!

notQuiteRight :: IO ()
notQuiteRight = do
  mv <- newEmptyMVar
  _ <- forkIO $ expensiveComputation_stricter mv
  -- some other activity
  result <- takeMVar mv
  print result

expensiveComputation_stricter :: MVar [Char] -> IO ()
expensiveComputation_stricter mv = do
  let a = "this is"
      b = "not really"
      c = "all that expensive"
  putMVar mv (a ++ b ++ c)
