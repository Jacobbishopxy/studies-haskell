-- file: Compressor.hs
-- author: Jacob Xie
-- date: 2024/06/19 22:27:36 Wednesday
-- brief:

import Codec.Compression.GZip (compress)
import Control.Concurrent (forkIO)
import Control.Exception (SomeException, handle)
import qualified Data.ByteString.Lazy as L

main :: IO ()
main = do
  putStrLn "Enter a file to compress>"
  name <- getLine
  handle (print :: SomeException -> IO ()) $ do
    content <- L.readFile name
    _ <- forkIO $ compressFile name content
    return ()
  where
    compressFile path = L.writeFile (path ++ ".gz") . compress
