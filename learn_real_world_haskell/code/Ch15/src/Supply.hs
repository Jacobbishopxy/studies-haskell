{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- file: Supply.hs
-- author: Jacob Xie
-- date: 2024/05/31 22:03:21 Friday
-- brief:

module Supply (Supply, next, runSupply) where

import Control.Monad.State

newtype Supply s a = S (State [s] a)

runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (S m) xs = runState m xs

next :: Supply s (Maybe s)
next = S $ do
  st <- get
  case st of
    [] -> return Nothing
    (x : xs) -> do
      put xs
      return $ Just x

unwrapS :: Supply s a -> State [s] a
unwrapS (S s) = s

instance Functor (Supply s) where
  fmap f s = S $ fmap f $ unwrapS s

instance Applicative (Supply s) where
  pure = S . return
  f <*> a = S $ unwrapS f <*> unwrapS a

instance Monad (Supply s) where
  s >>= m = S $ unwrapS s >>= unwrapS . m
