-- file: NiceFork.hs
-- author: Jacob Xie
-- date: 2024/04/22 09:17:32 Monday
-- brief:

module NiceFork
  ( ThreadManager,
    newManager,
    forkManaged,
    getStatus,
    waitFor,
    waitAll,
  )
where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Map as M

newtype ThreadManager = Mgr (MVar (M.Map ThreadId (MVar ThreadStatus))) deriving (Eq)

data ThreadStatus
  = Running
  | Finished
  | Threw IOException -- changed from `Exception`
  deriving (Eq, Show)

-- Create a new thread manager
newManager :: IO ThreadManager
newManager = Mgr <$> newMVar M.empty

-- Create a new managed thread and watch its status
forkManaged :: ThreadManager -> IO () -> IO ThreadId
forkManaged (Mgr mgr) body =
  -- safely modify an MVar
  modifyMVar mgr $ \m -> do
    state <- newEmptyMVar
    tid <- forkIO $ do
      result <- try body
      putMVar state $ either Threw (const Finished) result
    return (M.insert tid state m, tid)

-- Finding the status of a thread

-- Immediately return the status of a managed thread
getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus (Mgr mgr) tid =
  modifyMVar mgr $ \m ->
    case M.lookup tid m of
      Nothing -> return (m, Nothing)
      Just st -> tryTakeMVar st >>= mst m
  where
    mst m' mm = case mm of
      Nothing -> return (m', Just Running)
      Just sth -> return (M.delete tid m', Just sth)

-- Block until a specific managed thread terminates
waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
-- waitFor (Mgr mgr) tid = do
--   maybeDone <- modifyMVar mgr $ \m ->
--     return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
--       (Nothing, _) -> (m, Nothing)
--       (done, m') -> (m', done)
--   case maybeDone of
--     Nothing -> return Nothing
--     Just st -> Just <$> takeMVar st
waitFor (Mgr mgr) tid =
  join . modifyMVar mgr $ \m -> return $
    case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
      (Nothing, _) -> (m, return Nothing)
      (Just st, m') -> (m', Just <$> takeMVar st)

-- Block until all managed threads terminate
waitAll :: ThreadManager -> IO ()
waitAll (Mgr mgr) = modifyMVar mgr es >>= mapM_ takeMVar
  where
    es m = return (M.empty, M.elems m)
