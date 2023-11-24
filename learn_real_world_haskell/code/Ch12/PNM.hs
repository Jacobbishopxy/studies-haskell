-- file: PNM.hs
-- author: Jacob Xie
-- date: 2023/11/24 19:05:17 Friday
-- brief:

module PNM (Greymap (..)) where

import Data.ByteString.Lazy qualified as L
import Data.ByteString.Lazy.Char8 qualified as L8
import Data.Char (isSpace)

data Greymap = Greymap
  { greyWidth :: Int,
    greyHeight :: Int,
    greyMax :: Int,
    greyData :: L.ByteString
  }
  deriving (Eq)

instance Show Greymap where
  show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m
