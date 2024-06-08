{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- file: SupplyInstance.hs
-- author: Jacob Xie
-- date: 2024/06/07 21:41:01 Friday
-- brief:

module SupplyInstance
  ( module SupplyInstance,
  )
where

import Control.Monad
import SupplyClass

newtype MySupply e a = MySupply {runMySupply :: Reader e a}
  deriving (Functor, Applicative, Monad)

instance MonadSupply e (MySupply e) where
  next = MySupply $ Just `liftM` ask

xy :: (Num s, MonadSupply s m, MonadFail m) => m s
xy = do
  Just x <- next
  Just y <- next
  return $ x * y
