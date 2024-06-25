-- file: ModifyMVar.hs
-- author: Jacob Xie
-- date: 2024/06/25 22:49:30 Tuesday
-- brief:

import Control.Concurrent (MVar, putMVar, takeMVar)
import Control.Exception (evaluate, mask, onException)

modifyMVar' :: MVar a -> (a -> IO (a, b)) -> IO b
modifyMVar' m io =
  mask $ \restore -> do
    a <- takeMVar m
    (a', b) <- restore (io a >>= evaluate) `onException` putMVar m a
    putMVar m a'
    return b
