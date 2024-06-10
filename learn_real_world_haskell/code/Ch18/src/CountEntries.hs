-- file: CountEntries.hs
-- author: Jacob Xie
-- date: 2024/06/09 23:38:50 Sunday
-- brief:

module CountEntries
  ( listDirectory,
    countEntriesTrad,
  )
where

import Control.Monad (forM, liftM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

listDirectory :: FilePath -> IO [FilePath]
listDirectory = liftM (filter notDots) . getDirectoryContents
  where
    notDots p = p /= "." && p /= ".."

countEntriesTrad :: FilePath -> IO [(FilePath, Int)]
countEntriesTrad path = do
  contents <- listDirectory path
  rest <- forM contents $ \name -> do
    let newName = path </> name
    isDir <- doesDirectoryExist newName
    if isDir
      then countEntriesTrad newName
      else return []
  return $ (path, length contents) : concat rest
