-- file: CountEntriesT.hs
-- author: Jacob Xie
-- date: 2024/06/10 13:12:36 Monday
-- brief:

module CountEntriesT
  ( countEntries,
  )
where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer
import CountEntries (listDirectory)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \name -> do
    let newName = path </> name
    isDir <- liftIO . doesDirectoryExist $ newName
    when isDir $ countEntries newName
