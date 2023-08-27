-- file: DataEither.hs
-- author: Jacob Xie
-- date: 2023/08/26 23:58:37 Saturday
-- brief:

module DataEither where

data Maybe a
  = Nothing
  | Just a
  deriving (Eq, Ord, Read, Show)

data Either a b
  = Left a
  | Right b
  deriving (Eq, Ord, Read, Show)