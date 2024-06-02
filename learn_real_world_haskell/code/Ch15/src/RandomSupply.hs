-- file: RandomSupply.hs
-- author: Jacob Xie
-- date: 2024/06/02 00:00:29 Sunday
-- brief:

module RandomSupply
  ( module RandomSupply,
  )
where

-- import Supply
import System.Random hiding (next)

randomsIO :: (Random a) => IO [a]
randomsIO = getStdRandom $ \g ->
  let (a, b) = split g in (randoms a, b)
