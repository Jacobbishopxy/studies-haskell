{-# LANGUAGE BangPatterns #-}

-- file: ModifyMVarStrict.hs
-- author: Jacob Xie
-- date: 2024/06/26 23:46:21 Wednesday
-- brief:

import Control.Concurrent (MVar, putMVar, takeMVar)
import Control.Exception (mask, onException)

modifyMVar_strict :: MVar a -> (a -> IO a) -> IO ()
modifyMVar_strict m io = mask $ \restore -> do
  a <- takeMVar m
  !a' <- restore (io a) `onException` putMVar m a
  putMVar m a'
