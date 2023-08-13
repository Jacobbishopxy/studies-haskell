-- file: readerMonad.hs
-- author: Jacob Xie
-- date: 2023/08/13 10:27:22 Sunday
-- brief:

import Control.Monad
import Control.Monad.State
import System.Random

-- addStuff :: Int -> Int
-- addStuff = do
--   a <- (* 2)
--   b <- (+ 10)
--   return (a + b)

addStuff :: Int -> Int
addStuff x =
  let a = (* 2) x
      b = (+ 10) x
   in a + b

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, newGen'') = random newGen'
   in (firstCoin, secondCoin, thirdCoin)

type Stack = [Int]

-- pop :: Stack -> (Int, Stack)
-- pop (x : xs) = (x, xs)

-- push :: Int -> Stack -> ((), Stack)
-- push a xs = ((), a : xs)

-- stackManip :: Stack -> (Int, Stack)
-- stackManip stack =
--   let ((), newStack1) = push 3 stack
--       (a, newStack2) = pop newStack1
--    in pop newStack2

pop :: State Stack Int
pop = state $ \(x : xs) -> (x, xs)

push :: Int -> State Stack ()
push a = state $ \xs -> ((), a : xs)

stackManip :: State Stack Int
stackManip = do
  push 3
  a <- pop
  pop

stackStuff :: State Stack ()
stackStuff = do
  a <- pop
  if a == 5
    then push 5
    else do
      push 3
      push 8

moreStack :: State Stack ()
moreStack = do
  a <- stackManip
  -- if a == 100
  --     then stackStuff
  --     else return ()
  when (a == 100) stackStuff

stackyStack :: State Stack ()
stackyStack = do
  stackNow <- get
  if stackNow == [1, 2, 3]
    then put [8, 3, 1]
    else put [9, 2, 1]

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

threeCoins' :: State StdGen (Bool, Bool, Bool)
threeCoins' = do
  a <- randomSt
  b <- randomSt
  c <- randomSt
  return (a, b, c)

main :: IO ()
main = do
  putStrLn $ "stackManip: " ++ show (runState stackManip [5, 8, 2, 1])

  putStrLn $ "stackStuff: " ++ show (runState stackStuff [9, 0, 2, 1, 0])

  putStrLn $ "stackyStack: " ++ show (runState stackyStack [1, 2, 3])

  let (coins, states) = runState threeCoins' (mkStdGen 33)
  putStrLn $ "new threeCoins" ++ show coins ++ " " ++ show states
