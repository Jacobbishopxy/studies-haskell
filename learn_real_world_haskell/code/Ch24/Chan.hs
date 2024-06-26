-- file: Chan.hs
-- author: Jacob Xie
-- date: 2024/06/26 20:08:44 Wednesday
-- brief:

import Control.Concurrent (forkIO, newChan, readChan, writeChan)

chanExample :: IO ()
chanExample = do
  ch <- newChan
  _ <- forkIO $ do
    writeChan ch "hello world"
    writeChan ch "now i quit"
  readChan ch >>= print
  readChan ch >>= print
