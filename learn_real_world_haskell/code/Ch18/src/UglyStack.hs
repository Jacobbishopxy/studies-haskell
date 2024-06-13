{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- file: UglyStack.hs
-- author: Jacob Xie
-- date: 2024/06/12 20:28:48 Wednesday
-- brief:

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import System.Directory
import System.FilePath

data AppConfig = AppConfig
  { cfgMaxDepth :: Int
  }
  deriving (Show)

data AppState = AppState
  { stDeepestReached :: Int
  }
  deriving (Show)

type App = ReaderT AppConfig (StateT AppState IO)

runApp :: App a -> Int -> IO (a, AppState)
runApp k maxDepth =
  let cfg = AppConfig maxDepth
      stt = AppState 0
   in runStateT (runReaderT k cfg) stt

constrainedCount :: Int -> FilePath -> App [(FilePath, Int)]
constrainedCount curDepth path = do
  contents <- liftIO . listDirectory $ path
  cfg <- ask
  rest <- forM contents $ \name -> do
    let newPath = path </> name
    isDir <- liftIO $ doesDirectoryExist newPath
    if isDir && curDepth < cfgMaxDepth cfg
      then do
        let newDepth = curDepth + 1
        st <- get
        when (stDeepestReached st < newDepth) $ put st {stDeepestReached = newDepth}
        constrainedCount newDepth newPath
      else return []
  return $ (path, length contents) : concat rest

newtype MyApp a = MyA
  { runA :: ReaderT AppConfig (StateT AppState IO) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadReader AppConfig,
      MonadState AppState
    )

runMyApp :: MyApp a -> Int -> IO (a, AppState)
runMyApp k maxDepth =
  let cfg = AppConfig maxDepth
      stt = AppState 0
   in runStateT (runReaderT (runA k) cfg) stt

--

implicitGet :: App AppState
implicitGet = get

explicitGet :: App AppState
explicitGet = lift get
