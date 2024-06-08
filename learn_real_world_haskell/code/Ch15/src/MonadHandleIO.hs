{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- file: MonadHandleIO.hs
-- author: Jacob Xie
-- date: 2024/06/08 22:33:06 Saturday
-- brief:

module MonadHandleIO
  ( safeHello,
  )
where

import MonadHandle
import System.IO (IOMode (..))
import qualified System.IO as IO

instance MonadHandle IO.Handle IO where
  openFile = IO.openFile
  hPutStr = IO.hPutStr
  hClose = IO.hClose
  hGetContents = IO.hGetContents
  hPutStrLn = IO.hPutStrLn

safeHello :: (MonadHandle h m) => FilePath -> m ()
safeHello path = do
  h <- openFile path WriteMode
  hPutStrLn h "hello world"
  hClose h
