-- file: LockHierarchy.hs
-- author: Jacob Xie
-- date: 2024/06/27 09:16:45 Thursday
-- brief:

import Control.Concurrent (MVar, forkIO, modifyMVar_, newMVar, yield)

nestedModification :: (Num a1, Num a2) => MVar a2 -> MVar a1 -> IO ()
nestedModification outer inner = do
  modifyMVar_ outer $ \x -> do
    yield -- force this thread to temporarily yield the CPU
    modifyMVar_ inner $ \y -> return $ y + 1
    return $ x + 1
  putStrLn "done"

main :: IO ()
main = do
  a <- newMVar (1 :: Int)
  b <- newMVar (2 :: Int)
  _ <- forkIO $ nestedModification a b
  _ <- forkIO $ nestedModification b a

  return ()
