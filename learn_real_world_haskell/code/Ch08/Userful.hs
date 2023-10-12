-- file: Userful.hs
-- author: Jacob Xie
-- date: 2023/10/12 21:57:28 Thursday
-- brief:

import Glob (namesMatching)
import System.Directory (doesFileExist, renameDirectory, renameFile)
import System.FilePath (replaceExtension, replaceExtensions)

renameWith :: (FilePath -> FilePath) -> FilePath -> IO FilePath
renameWith f path = do
  let path' = f path
  rename path path'
  return path'

rename :: FilePath -> FilePath -> IO ()
rename old new = do
  isFile <- doesFileExist old
  let f = if isFile then renameFile else renameDirectory
  f old new

cc2cpp :: IO [FilePath]
-- cc2cpp = mapM (renameWith (flip replaceExtension ".cpp")) =<< namesMatching "*.cc"
cc2cpp = mapM (renameWith (`replaceExtension` ".cpp")) =<< namesMatching "*.cc"
