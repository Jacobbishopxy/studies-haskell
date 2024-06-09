{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- file: MonadHandleIO.hs
-- author: Jacob Xie
-- date: 2024/06/08 22:33:06 Saturday
-- brief:

module MonadHandleIO
  ( safeHello,
    tidierHello,
    tidyHello,
  )
where

import Control.Monad.Trans
import MonadHandle
import System.Directory
import System.IO (Handle, IOMode (..))
import qualified System.IO as IO

instance MonadHandle IO.Handle IO where
  openFile = IO.openFile
  hPutStr = IO.hPutStr
  hClose = IO.hClose
  hGetContents = IO.hGetContents
  hPutStrLn = IO.hPutStrLn

-- test
safeHello :: (MonadHandle h m) => FilePath -> m ()
safeHello path = do
  h <- openFile path WriteMode
  hPutStrLn h "hello world"
  hClose h

-- restriction
class (MonadHandle h m, MonadIO m) => MonadHandleIO h m | m -> h

instance MonadHandleIO Handle IO

tidierHello :: (MonadHandleIO h m) => FilePath -> m ()
tidierHello path = do
  safeHello path
  liftIO $ removeFile path

tidyHello :: (MonadIO m, MonadHandle h m) => FilePath -> m ()
tidyHello path = do
  safeHello path
  liftIO $ removeFile path
