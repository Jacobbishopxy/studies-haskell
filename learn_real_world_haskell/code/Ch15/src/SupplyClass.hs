{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- file: SupplyClass.hs;
-- author: Jacob Xie
-- date: 2024/06/02 19:01:59 Sunday
-- brief:

module SupplyClass
  ( MonadSupply (..),
    S.Supply,
    S.runSupply,
    showTwoClass,
    Reader,
    ask,
  )
where

import qualified Supply as S

class (Monad m) => MonadSupply s m | m -> s where
  next :: m (Maybe s)

instance MonadSupply s (S.Supply s) where
  next = S.next

showTwoClass :: (Show s, Monad m, MonadSupply s m) => m String
showTwoClass = do
  a <- next
  b <- next
  return $ show "a: " ++ show a ++ ", b: " ++ show b

newtype Reader e a = R {runReader :: e -> a}

instance Functor (Reader a) where
  fmap f m = R $ f . runReader m

instance Applicative (Reader a) where
  pure = R . const
  f <*> m = R $ \r -> runReader f r $ runReader m r

instance Monad (Reader e) where
  m >>= k = R $ \r -> runReader (k $ runReader m r) r

ask :: Reader e e
ask = R id
