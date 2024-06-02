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
  )
where

import qualified Supply as S

class (Monad m) => MonadSupply s m | m -> s where
  next :: m (Maybe s)

instance MonadSupply s (S.Supply s) where
  next = S.next
