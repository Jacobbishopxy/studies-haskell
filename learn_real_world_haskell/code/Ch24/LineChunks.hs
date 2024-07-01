-- file: LineChunks.hs
-- author: Jacob Xie
-- date: 2024/06/30 20:37:11 Sunday
-- brief:

module LineChunks
  ( chunkedReadWith,
  )
where

import Control.Exception (bracket, finally)
import Control.Parallel.Strategies (NFData, rdeepseq)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Int (Int64)
import GHC.Conc (numCapabilities)
import GHC.IO.Handle (Handle, hClose)

data ChunkSpec = CS
  { chunkOffset :: !Int64,
    chunkLength :: !Int64
  }
  deriving (Eq, Show)

withChunks ::
  (NFData a) =>
  (FilePath -> IO [ChunkSpec]) ->
  ([LB.ByteString] -> a) ->
  FilePath ->
  IO a
withChunks chunkFunc process path = do
  (chunks, handles) <- chunkedRead chunkFunc path
  let r = process chunks
  (rdeepseq r `seq` return r) `finally` mapM_ hClose handles

chunkedReadWith :: (NFData a) => ([LB.ByteString] -> a) -> FilePath -> IO a
chunkedReadWith func path =
  withChunks (lineChunks (numCapabilities * 4)) func path

chunkedRead :: (FilePath -> IO [ChunkSpec]) -> FilePath -> IO ([LB.ByteString], [Handle])
chunkedRead = undefined

lineChunks :: Int -> FilePath -> IO [ChunkSpec]
lineChunks = undefined
