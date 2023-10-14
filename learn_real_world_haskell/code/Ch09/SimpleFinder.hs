-- file: SimpleFinder.hs
-- author: Jacob Xie
-- date: 2023/10/14 21:57:59 Saturday
-- brief:

import RecursiveContents (getRecursiveContents)

simpleFind :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
simpleFind p path = do
  names <- getRecursiveContents path
  return $ filter p names
