-- file: BetterPredicate.hs
-- author: Jacob Xie
-- date: 2023/10/15 00:37:20 Sunday
-- brief:

import Control.Exception (SomeException, bracket, handle)
import Control.Monad (filterM)
import Data.Time.Clock (UTCTime)
import GHC.IO.Handle.FD (withFile)
import RecursiveContents (getRecursiveContents)
import System.Directory (Permissions (..), getModificationTime, getPermissions)
import System.FilePath (takeExtensions)
import System.IO (IOMode (..), hClose, hFileSize, openFile)

type Predicate =
  FilePath ->
  Permissions ->
  Maybe Integer ->
  UTCTime ->
  Bool

-- stub fn
-- getFileSize :: FilePath -> IO (Maybe Integer)
-- getFileSize = undefined

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where
    check name = do
      perms <- getPermissions name
      size <- getFileSize name
      modified <- getModificationTime name
      return $ p name perms size modified

simpleFileSize :: FilePath -> IO Integer
simpleFileSize path = do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return size

saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path = handle errorHandler $ do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return $ Just size
  where
    errorHandler :: SomeException -> IO (Maybe Integer)
    errorHandler _ = return Nothing

-- ////////////////////////////////////////////////////////////////////////////////////////////////

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle errorHandler $
  -- bracket (openFile path ReadMode) hClose $ \h -> do
  withFile path ReadMode $ \h -> do
    size <- hFileSize h
    return (Just size)
  where
    errorHandler :: SomeException -> IO (Maybe Integer)
    errorHandler _ = return Nothing
