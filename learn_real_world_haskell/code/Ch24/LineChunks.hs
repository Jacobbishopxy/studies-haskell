-- file: LineChunks.hs
-- author: Jacob Xie
-- date: 2024/06/30 20:37:11 Sunday
-- brief:

module LineChunks
  ( chunkedReadWith,
  )
where

import Control.Exception (bracket, finally)
import Control.Monad
import Control.Parallel.Strategies (NFData, rdeepseq)
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.Int (Int64)
import GHC.Conc (numCapabilities)
import System.IO

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
chunkedRead chunkFunc path = do
  chunks <- chunkFunc path
  liftM unzip . forM chunks $ \spec -> do
    h <- openFile path ReadMode
    hSeek h AbsoluteSeek (fromIntegral $ chunkOffset spec)
    chunk <- LB.take (chunkLength spec) `liftM` LB.hGetContents h
    return (chunk, h)

lineChunks :: Int -> FilePath -> IO [ChunkSpec]
lineChunks numChunks path = do
  bracket (openFile path ReadMode) hClose $ \h -> do
    totalSize <- fromIntegral `liftM` hFileSize h
    let chunkSize = totalSize `div` fromIntegral numChunks
        findChunks offset = do
          let newOffset = offset + chunkSize
          hSeek h AbsoluteSeek (fromIntegral newOffset)
          let findNewline off = do
                eof <- hIsEOF h
                if eof
                  then return [CS offset (totalSize - offset)]
                  else do
                    bytes <- LB.hGet h 4096
                    case LB.elemIndex '\n' bytes of
                      Just n -> do
                        chunks@(c : _) <- findChunks (off + n + 1)
                        let coff = chunkOffset c
                        return (CS offset (coff - offset) : chunks)
                      Nothing -> findNewline (off + LB.length bytes)
          findNewline newOffset
    findChunks 0
