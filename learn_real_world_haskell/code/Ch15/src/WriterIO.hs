{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- file: WriterIO.hs
-- author: Jacob Xie
-- date: 2024/06/09 09:36:57 Sunday
-- brief:

module WriterIO
  ( Event (..),
    WriterIO,
    runWriterIO,
    testRun,
  )
where

import Control.Monad.Writer
import MonadHandle (MonadHandle (..))
import MonadHandleIO
import System.IO

data Event
  = Open FilePath IOMode
  | Put String String
  | Close String
  | GetContents String
  deriving (Show)

newtype WriterIO a = W {runW :: Writer [Event] a}
  deriving (Functor, Applicative, Monad, MonadWriter [Event])

runWriterIO :: WriterIO a -> (a, [Event])
runWriterIO = runWriter . runW

----------------------------------------------------------------------------------------------------

instance MonadHandle FilePath WriterIO where
  openFile path mode = tell [Open path mode] >> return path
  hPutStr h str = tell [Put h str]
  hClose h = tell [Close h]
  hGetContents h = tell [GetContents h] >> return "fake contents"

testRun :: FilePath -> ((), [Event])
testRun s = runWriterIO (safeHello s)
