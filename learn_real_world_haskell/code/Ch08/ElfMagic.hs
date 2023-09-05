-- file: ElfMagic.hs
-- author: Jacob Xie
-- date: 2023/09/05 20:29:10 Tuesday
-- brief:

import Data.ByteString.Lazy qualified as L

hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
  where
    elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]

--
isElfFile :: FilePath -> IO Bool
isElfFile path = do
  content <- L.readFile path
  return $ hasElfMagic content
