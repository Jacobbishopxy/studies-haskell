-- file: Prettify2.hs
-- author: Jacob Xie
-- date: 2023/11/20 14:00:58 Monday
-- brief:

module Prettify2 (Doc (..)) where

data Doc
  = Empty
  | Char Char
  | Text String
  | Line
  | Concat Doc Doc
  | Union Doc Doc
  deriving (Show, Eq)

--
