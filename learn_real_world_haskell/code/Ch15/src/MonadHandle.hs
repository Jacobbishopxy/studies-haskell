{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- file: MonadHandle.hs
-- author: Jacob Xie
-- date: 2024/06/08 21:38:01 Saturday
-- brief:

module MonadHandle
  ( MonadHandle (..),
  )
where

import qualified System.IO as IO

class (Monad m) => MonadHandle h m | m -> h where
  openFile :: FilePath -> IO.IOMode -> m h
  hPutStr :: h -> String -> m ()
  hClose :: h -> m ()
  hGetContents :: h -> m String

  hPutStrLn :: h -> String -> m ()
  hPutStrLn h s = hPutStr h s >> hPutStr h "\n"
