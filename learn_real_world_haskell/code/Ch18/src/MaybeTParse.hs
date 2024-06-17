{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- file: MaybeTParse.hs
-- author: Jacob Xie
-- date: 2024/06/16 23:29:39 Sunday
-- brief:

module MaybeTParse
  ( Parse,
    evalParse,
  )
where

import Control.Monad.State
import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)
import MaybeT

data ParseState = ParseState
  { string :: L.ByteString,
    offset :: Int64
  }
  deriving (Show)

newtype Parse a = P
  { runP :: MaybeT (State ParseState) a
  }
  deriving (Functor, Applicative, Monad, MonadState ParseState)

evalParse :: Parse a -> L.ByteString -> Maybe a
evalParse m s = evalState (runMaybeT $ runP m) (ParseState s 0)
