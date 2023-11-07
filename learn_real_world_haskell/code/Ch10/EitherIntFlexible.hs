-- file: EitherIntFlexible.hs
-- author: Jacob Xie
-- date: 2023/11/07 21:26:14 Tuesday
-- brief:
-- {-# LANGUAGE FlexibleInstances #-}

-- instance Functor (Either Int) where
--   fmap _ (Left n) = Left n
--   fmap f (Right r) = Right (f r)

-- already defined in Prelude
-- instance Functor (Either a) where
--   fmap f (Right x) = Right (f x)
--   fmap f (Left x) = Left x
