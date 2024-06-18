{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

-- file: MaybeT.hs
-- author: Jacob Xie
-- date: 2024/06/14 21:26:40 Friday
-- brief:

module MaybeT
  ( module MaybeT,
  )
where

import Control.Monad
import Control.Monad.State
import Control.Monad.Writer

newtype MaybeT m a = MaybeT
  { runMaybeT :: m (Maybe a)
  }

bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
x `bindMT` f = MaybeT $ do
  unwrapped <- runMaybeT x
  case unwrapped of
    Nothing -> return Nothing
    Just y -> runMaybeT (f y)

altBindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
x `altBindMT` f =
  MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)

instance (Functor f) => Functor (MaybeT f) where
  fmap f x = MaybeT $ fmap f <$> runMaybeT x

instance (Applicative a) => Applicative (MaybeT a) where
  pure = MaybeT . pure . Just
  f <*> x = MaybeT $ fmap (<*>) (runMaybeT f) <*> runMaybeT x

instance (Monad m) => Monad (MaybeT m) where
  (>>=) = bindMT

instance (MonadFail m) => MonadFail (MaybeT m) where
  fail _ = MaybeT $ return Nothing

instance MonadTrans MaybeT where
  lift m = MaybeT $ Just `liftM` m

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

instance (MonadState s m) => MonadState s (MaybeT m) where
  get = lift get
  put = lift . put

instance (Monoid w, MonadWriter w m) => MonadWriter w (MaybeT m) where
  tell = lift . tell
  listen m = MaybeT $ do
    (result, l) <- listen $ runMaybeT m
    case result of
      Nothing -> return Nothing
      Just v -> return $ Just (v, l)
  pass m = MaybeT $ do
    result <- runMaybeT m
    case result of
      Nothing -> return Nothing
      Just (v, l) -> pass $ return (Just v, l)
