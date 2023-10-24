-- file: ControlledVisit.hs
-- author: Jacob Xie
-- date: 2023/10/22 19:31:48 Sunday
-- brief:

module ControlledVisit (Info, getUsefulContents, getInfo, isDirectory, takeFileName, infoPath) where

import Control.Exception (SomeException (SomeException), bracket, handle)
import Control.Monad (liftM)
import Data.Time (UTCTime)
import Data.Traversable (forM)
import GHC.IO.Handle (hClose, hFileSize)
import System.Directory (Permissions (searchable), getDirectoryContents, getModificationTime, getPermissions)
import System.FilePath (takeFileName, (</>))
import System.IO (IOMode (ReadMode), openFile, withFile)

data Info = Info
  { infoPath :: FilePath,
    infoPerms :: Maybe Permissions,
    infoSize :: Maybe Integer,
    infoModTime :: Maybe UTCTime
  }
  deriving (Eq, Ord, Show)

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO (getPermissions path)
  -- size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  size <- maybeIO (withFile path ReadMode hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)

maybeIO :: IO a -> IO (Maybe a)
-- explicit input type `SomeException _`
maybeIO act = handle (\(SomeException _) -> return Nothing) $ Just `liftM` act

-- ////////////////////////////////////////////////////////////////////////////////////////////////

traverse' :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse' order path = do
  names <- getUsefulContents path
  -- from right to left
  -- concat all `names` with the given `path`
  -- prepend current directory to every element of the list, and includes current directory itself
  -- `mapM` apply `getInfo` to this Monad result
  contents <- mapM getInfo (path : map (path </>) names)
  -- `liftM` promote a fn (`concat`) to a Monad (IO)
  -- `forM`, flipped `mapM`, applies anonymous function on `order contents`
  -- `order` is an user-supplied traversal control function
  liftM concat $ forM (order contents) $ \info -> do
    if isDirectory info && infoPath info /= path
      then traverse' order (infoPath info)
      else return [info]

-- list of directory entries
-- exclude self and parent directory
getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

-- ////////////////////////////////////////////////////////////////////////////////////////////////
-- less experienced Haskell programmer

traverseVerbose order path = do
  names <- getDirectoryContents path
  let usefulNames = filter (`notElem` [".", ".."]) names
  contents <- mapM getEntryName ("" : usefulNames)
  recursiveContents <- mapM recurse (order contents)
  return (concat recursiveContents)
  where
    getEntryName name = getInfo (path </> name)
    -- isDirectory info = case infoPerms info of
    --   Nothing -> False
    --   Just perms -> searchable perms
    isDirectory info = maybe False searchable (infoPerms info)
    recurse info = do
      if isDirectory info && infoPath info /= path
        then traverseVerbose order (infoPath info)
        else return [info]
