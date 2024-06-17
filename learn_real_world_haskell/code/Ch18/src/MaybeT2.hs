-- file: MaybeT2.hs
-- author: Jacob Xie
-- date: 2024/06/15 11:02:12 Saturday
-- brief:

import Control.Monad (guard)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State

{-
  Prerequisites:

  Monad:
    - program-wide concerns -> state, IO, exceptions and etc.
    - a container, which not only hold value, but also defines how those values can be transformed & combined

  Monad Transformers:
    - handling Monads' combination
    - stack Monads: layer multiple effects together in a single computation
-}

----------------------------------------------------------------------------------------------------
-- Example
--
-- MaybeT:
--  handling computations that might fail to another monad. It wraps another monad w/ 'Maybe'
-- StateT:
--  state-handling capability to another monad. It wraps another monad w/ state operations
----------------------------------------------------------------------------------------------------

-- ensures both integers are non-negative before adding them, using the `Maybe` monad to handle the potential failure
maybeAdd :: Int -> Int -> Maybe Int
maybeAdd x y = do
  guard $ x >= 0 && y >= 0
  return $ x + y

{-
  ghci> :l MaybeT2.hs
  [1 of 2] Compiling Main             ( MaybeT2.hs, interpreted )
  Ok, one module loaded.
  ghci> maybeAdd 3 5
  Just 8
  ghci> maybeAdd (-1) 5
  Nothing
-}

type MaybeState s a = MaybeT (State s) a

maybeAddWithState :: Int -> Int -> MaybeState Int Int
maybeAddWithState x y = do
  guard (x >= 0 && y >= 0)
  lift $ modify (+ 1) -- modify the state
  return $ x + y

{-
  ghci> :set -package mtl
  package flags have changed, resetting and loading new packages...
  ghci> :l MaybeT2.hs
  [1 of 2] Compiling Main             ( MaybeT2.hs, interpreted )
  Ok, one module loaded.
  ghci> runState (runMaybeT (maybeAddWithState 3 5)) 0
  (Just 8,1)
  ghci> runState (runMaybeT (maybeAddWithState (-1) 5)) 0
  (Nothing,0)
-}

----------------------------------------------------------------------------------------------------
-- Key concepts and operations
----------------------------------------------------------------------------------------------------

-- 1. Lift:
-- `lift` function (from `mtl`'s Trans) is used to bring operations from the inner monad to the transformer stack.
-- Think of it as pulling the inner layer's functionality to the outer layer.
-- `lift :: (MonadTrans t, Monad m) => m a -> t m a`
--
-- 2. Run Functions:
-- Each monad transformer has a run function to extract the computations.
--  a) `runMaybeT :: MaybeT m a -> m (Maybe a)`
--  b) `runStateT :: StateT s m a -> s -> m (a, s)`
