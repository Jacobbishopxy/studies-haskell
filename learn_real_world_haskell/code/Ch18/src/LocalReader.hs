{-# LANGUAGE FlexibleContexts #-}

-- file: LocalReader.hs
-- author: Jacob Xie
-- date: 2024/06/10 18:14:30 Monday
-- brief:

import Control.Monad.Reader

myName :: (MonadReader String m) => String -> m String
myName step = do
  name <- ask
  return (step ++ ", I am " ++ name)

localExample :: Reader String (String, String, String)
localExample = do
  a <- myName "First"
  b <- local (++ "dy") (myName "Second")
  c <- myName "Third"
  return (a, b, c)
