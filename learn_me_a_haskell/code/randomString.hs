-- file: randomString.hs
-- author: Jacob Xie
-- date: 2023/07/28 21:37:00 Friday
-- brief:

import System.Random

-- simple
-- main = do
--   gen <- getStdGen
--   putStr $ take 20 $ randomRs ('a', 'z') gen

-- print the same string twice!
-- main = do
--     gen <- getStdGen
--     putStrLn $ take 20 (randomRs ('a','z') gen)
--     gen2 <- getStdGen
--     putStr $ take 20 (randomRs ('a','z') gen2)

-- different strings
-- main = do
--   gen <- getStdGen
--   let randomChars = randomRs ('a', 'z') gen
--       (first20, rest) = splitAt 20 randomChars
--       (second20, _) = splitAt 20 rest
--   putStrLn first20
--   putStr second20

-- another way
main = do
  gen <- getStdGen
  putStrLn $ take 20 $ randomRs ('a', 'z') gen
  gen' <- newStdGen
  putStrLn $ take 20 $ randomRs ('a', 'z') gen'
