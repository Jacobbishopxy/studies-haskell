-- file: Glob.hs
-- author: Jacob Xie
-- date: 2023/10/12 18:14:03 Thursday
-- brief:

module Glob (namesMatching) where

import Control.Exception (SomeException, handle)
import Control.Monad (forM)
import GlobRegex (matchesGlob)
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>))

namesMatching pat
  | not (isPattern pat) = do
      exists <- doesNameExist pat
      -- return (if exists then [pat] else [])
      return ([pat | exists])
  | otherwise = do
      case splitFileName pat of
        ("", baseName) -> do
          curDir <- getCurrentDirectory
          listMatches curDir baseName
        (dirName, baseName) -> do
          dirs <-
            if isPattern dirName
              then namesMatching (dropTrailingPathSeparator dirName)
              else return [dirName]
          let listDir =
                if isPattern baseName
                  then listMatches
                  else listPlain
          pathNames <- forM dirs $ \dir -> do
            baseNames <- listDir dir baseName
            return (map (dir </>) baseNames)
          return (concat pathNames)

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
  fileExists <- doesFileExist name
  if fileExists
    then return True
    else doesDirectoryExist name

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
  dirName' <-
    if null dirName
      then getCurrentDirectory
      else return dirName
  handle errorHandler $ do
    names <- getDirectoryContents dirName'
    let names' =
          if isHidden pat
            then filter isHidden names
            else filter (not . isHidden) names
    return (filter (`matchesGlob` pat) names')
  where
    errorHandler :: SomeException -> IO [String]
    errorHandler _ = return []

isHidden :: [Char] -> Bool
isHidden ('.' : _) = True
isHidden _ = False

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
  exists <-
    if null baseName
      then doesDirectoryExist dirName
      else doesNameExist (dirName </> baseName)
  -- return (if exists then [baseName] else [])
  return ([baseName | exists])
