-- file: ownRandom.hs
-- author: Jacob Xie
-- date: 2023/07/27 21:16:18 Thursday
-- brief:

import System.Environment
import System.Random

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, newGen) = random gen in value : randoms' newGen

finiteRandoms :: (RandomGen g, Random a, Num n, Eq n) => n -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
  let (value, newGen) = random gen
      (restOfList, finalGen) = finiteRandoms (n - 1) newGen
   in (value : restOfList, finalGen)

main = do
  args <- getArgs
  let (x1 : x2 : _) = args
  let res = finiteRandoms (read x1) (mkStdGen $ read x2) :: ([Int], StdGen)

  print res
