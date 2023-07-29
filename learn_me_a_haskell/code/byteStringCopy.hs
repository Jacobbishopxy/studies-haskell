-- file: byteStringCopy.hs
-- author: Jacob Xie
-- date: 2023/07/29 10:29:58 Saturday
-- brief:

import Data.ByteString.Lazy qualified as B
import System.Environment

main = do
  (fileName1 : fileName2 : _) <- getArgs
  copyFile fileName1 fileName2

copyFile :: FilePath -> FilePath -> IO ()
copyFile source dest = do
  contents <- B.readFile source
  B.writeFile dest contents
