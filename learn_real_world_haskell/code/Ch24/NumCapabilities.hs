-- file: NumCapabilities.hs
-- author: Jacob Xie
-- date: 2024/06/27 23:06:26 Thursday
-- brief:

import GHC.Conc (numCapabilities)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  putStrLn $ "command line arguments: " <> show args
  putStrLn $ "number of cores: " <> show numCapabilities
