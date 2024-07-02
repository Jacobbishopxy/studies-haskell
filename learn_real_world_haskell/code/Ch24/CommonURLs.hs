-- file: CommonURLs.hs
-- author: Jacob Xie
-- date: 2024/07/02 23:54:19 Tuesday
-- brief:

-- brew install pcre
-- `cabal.project.local`
{-
  package pcre-light
    extra-include-dirs: /opt/homebrew/opt/pcre/include
    extra-lib-dirs: /opt/homebrew/opt/pcre/lib
-}

module Main where

import Control.Monad (forM_)
import Control.Parallel.Strategies (rseq)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List (foldl', sortBy)
import qualified Data.Map as M
import LineChunks (chunkedReadWith)
import MapReduce (mapReduce)
import System.Environment (getArgs)
import Text.Regex.PCRE.Light (compile, match)

countURLs :: [L.ByteString] -> M.Map S.ByteString Int
countURLs = mapReduce rseq (foldl' augment M.empty . L.lines) rseq M.unions
  where
    augment m line =
      case match (compile pttn []) (strict line) [] of
        Just (_ : url : _) -> M.insertWith (+) url 1 m
        _ -> m
    strict = S.concat . L.toChunks
    pttn = S.pack "\"(?:GET|POST|HEAD) ([^ ]+) HTTP/"

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \path -> do
    m <- chunkedReadWith countURLs path
    let mostPopular (_, a) (_, b) = compare b a
    mapM_ print . take 10 . sortBy mostPopular . M.toList $ m
