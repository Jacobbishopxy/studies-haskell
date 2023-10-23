-- file: BetterPredicate.hs
-- author: Jacob Xie
-- date: 2023/10/15 00:37:20 Sunday
-- brief:

import Control.Exception (SomeException, bracket, handle)
import Control.Monad (filterM)
import Data.Time (UTCTime)
import GHC.IO.Handle.FD (withFile)
import RecursiveContents (getRecursiveContents)
import System.Directory (Permissions (..), getModificationTime, getPermissions)
import System.FilePath (takeExtension, takeExtensions)
import System.IO (IOMode (ReadMode), hClose, hFileSize, openFile)

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

  -- bracket (openFile path ReadMode) hClose $ \h -> do
  withFile path ReadMode $ \h -> do
    size <- hFileSize h
    return (Just size)
  where
    errorHandler :: SomeException -> IO (Maybe Integer)
    errorHandler _ = return Nothing

-- ////////////////////////////////////////////////////////////////////////////////////////////////
-- embedded domain specific language

myTest :: (Ord a, Num a) => FilePath -> p1 -> Maybe a -> p2 -> Bool
myTest path _ (Just size) _ = takeExtension path == ".cpp" && size > 131072
myTest _ _ _ _ = False

-- explicit type announcing
type InfoP a =
  FilePath -> -- path to directory entry
  Permissions -> -- permissions
  Maybe Integer -> -- file size (Nothing if not file)
  UTCTime -> -- last modified
  a

pathP :: InfoP FilePath
pathP path _ _ _ = path

--
sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
-- equalP f k = \w x y z -> f w x y z == k
equalP f k w x y z = f w x y z == k

-- equalP' :: (Eq a) => InfoP a -> a -> InfoP Bool
-- equalP' f k w x y z = f w x y z == k

-- avoid boilerplate w/ lifting
liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

-- gluing predicates together
simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool
simpleAndP f g w x y z = f w x y z && g w x y z

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP = liftP2 (&&)

orP :: InfoP Bool -> InfoP Bool -> InfoP Bool
orP = liftP2 (||)

constP :: a -> InfoP a
constP k _ _ _ _ = k

liftP' q f k w x y z = f w x y z `q` constP k w x y z

-- myTest2, use combinator
liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

myTest2 = (liftPath takeExtension `equalP` ".cpp") `andP` (sizeP `greaterP` 131072)

-- infix operators

-- explicit annotation
(==?) :: InfoP String -> String -> InfoP Bool
(==?) = equalP

(&&?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
(&&?) = andP

-- explicit annotation
(>?) :: InfoP Integer -> Integer -> InfoP Bool
(>?) = greaterP

myTest3 = (liftPath takeExtension ==? ".cpp") &&? (sizeP >? 131072)

-- parenthesis-free expression
infix 4 ==?

infixr 3 &&?

infix 4 >?

myTest4 = liftPath takeExtension ==? ".cpp" &&? sizeP >? 131072
