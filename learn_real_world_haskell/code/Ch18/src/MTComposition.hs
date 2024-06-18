{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- file: MTComposition.hs
-- author: Jacob Xie
-- date: 2024/06/17 20:51:25 Monday
-- brief:

import Control.Monad.Identity
import Control.Monad.Writer
import MaybeT

----------------------------------------------------------------------------------------------------

problem :: (MonadWriter [String] m, MonadFail m) => m ()
problem = do
  tell ["this is where i fail"]
  fail "oops"

type A = WriterT [String] Maybe

type B = MaybeT (Writer [String])

a :: A ()
a = problem

instance MonadFail Identity where
  fail _ = Identity $ error "oops"

b :: B ()
b = problem
