-- file: Random.hs
-- author: Jacob Xie
-- date: 2024/05/13 23:10:10 Monday
-- brief:

import Control.Monad (liftM2)
import Control.Monad.State
import System.Random

rand :: IO Int
rand = getStdRandom $ randomR (0, maxBound)

twoBadRandoms :: (RandomGen g) => g -> (Int, Int)
twoBadRandoms gen = (fst $ random gen, fst $ random gen)

twoGoodRandoms :: (RandomGen g) => g -> ((Int, Int), g)
twoGoodRandoms gen =
  let (a, gen') = random gen
      (b, gen'') = random gen'
   in ((a, b), gen'')

type RandomState a = State StdGen a

getRandom :: (Random a) => RandomState a
getRandom =
  get >>= \gen ->
    let (val, gen') = random gen
     in put gen' >> return val

getTwoRandoms :: (Random a) => RandomState (a, a)
getTwoRandoms = liftM2 (,) getRandom getRandom

runTwoRandoms :: IO (Int, Int)
runTwoRandoms = do
  oldState <- getStdGen
  let (result, newState) = runState getTwoRandoms oldState
  setStdGen newState
  return result

----------------------------------------------------------------------------------------------------

data CountedRandom = CountedRandom
  { crGen :: StdGen,
    crCount :: Int
  }

type CRState = State CountedRandom

getCountedRandom :: (Random a) => CRState a
getCountedRandom = do
  st <- get
  let (val, gen') = random (crGen st)
  put CountedRandom {crGen = gen', crCount = crCount st + 1}
  return val

getCount :: CRState Int
getCount = gets crCount

putCount :: Int -> CRState ()
putCount a = do
  st <- get
  put st {crCount = a}

putCountModify :: Int -> CRState ()
putCountModify a = modify $ \st -> st {crCount = a}
