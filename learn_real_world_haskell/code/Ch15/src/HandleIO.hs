{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- file: HandleIO.hs
-- author: Jacob Xie
-- date: 2024/06/08 09:55:37 Saturday
-- brief:

module HandleIO
  ( module HandleIO,
  )
where

import Control.Monad.Trans (MonadIO (..))
import System.Directory (removeFile)
import qualified System.IO as IO

newtype HandleIO a = HandleIO {runHandleIO :: IO a}
  deriving (Functor, Applicative, Monad)

openFile :: FilePath -> IO.IOMode -> HandleIO IO.Handle
openFile path mode = HandleIO $ IO.openFile path mode

hClose :: IO.Handle -> HandleIO ()
hClose = HandleIO . IO.hClose

hPutStrLn :: IO.Handle -> String -> HandleIO ()
hPutStrLn h s = HandleIO $ IO.hPutStrLn h s

safeHello :: FilePath -> HandleIO ()
safeHello path = do
  h <- openFile path IO.WriteMode
  hPutStrLn h "hello world"
  hClose h

instance MonadIO HandleIO where
  liftIO = HandleIO

tidyHello :: FilePath -> HandleIO ()
tidyHello path = do
  safeHello path
  liftIO $ removeFile path
