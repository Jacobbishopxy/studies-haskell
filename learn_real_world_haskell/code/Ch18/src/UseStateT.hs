-- file: UseStateT.hs
-- author: Jacob Xie
-- date: 2024/06/17 14:39:00 Monday
-- brief:

import Control.Monad.State
import Control.Monad.Trans.Writer
import Data.DList (DList)
import qualified Data.DList as DL

-- Difference List (DList)
-- optimizing append operations

-- Define type aliases for clarity
type Log = DList String

type StateCount = Int

-- Combined monad type using `StateT` over `Writer`
type WriterState a = StateT StateCount (Writer Log) a

-- Function to log a message
-- lifting `tell` into the `WriterState` monad, and using `DL.singleton` to create a single-element `DList`
-- `tell` function is used to add a log entry, it appends the log message to the log maintained by the `Writer` monad
-- `tell :: (Monad m, Monoid w) => w -> WriterT w m ()`
-- `DL.singleton msg` creates a `DList` containing just the msg string.
-- This is efficient because appending single elements to a DList is an O(1) operation.
logMessage :: String -> WriterState ()
logMessage msg = lift $ tell (DL.singleton msg)

-- Function to increment the state (operation count)
incrementCount :: WriterState ()
incrementCount = modify (+ 1)

-- Function to add two numbers
add :: Int -> Int -> WriterState Int
add x y = do
  let result = x + y
  incrementCount
  logMessage $ "Added " ++ show x ++ " and " ++ show y ++ ": result is " ++ show result
  return result

-- Function to multiply two numbers
multiply :: Int -> Int -> WriterState Int
multiply x y = do
  let result = x * y
  incrementCount
  logMessage $ "Multiplied " ++ show x ++ " and " ++ show y ++ ": result is " ++ show result
  return result

-- test
test :: WriterState Int
test = do
  a <- add 3 5
  b <- multiply a 2
  c <- add b 7
  return c

main :: IO ()
main = do
  let initState = 0
  let ((res, finalState), l) = runWriter (runStateT test initState)

  putStrLn $ "Result: " ++ show res
  putStrLn $ "Final State (Operation Count): " ++ show finalState
  putStrLn "Log:"
  mapM_ putStrLn $ DL.toList l

{-
  ghci> :set -package mtl
  package flags have changed, resetting and loading new packages...
  ghci> :set -package dlist
  package flags have changed, resetting and loading new packages...
  ghci> :l UseStateT.hs
  [1 of 2] Compiling Main             ( UseStateT.hs, interpreted )
  Ok, one module loaded.
  ghci> main
  Result: 23
  Final State (Operation Count): 3
  Log:
  Added 3 and 5: result is 8
  Multiplied 8 and 2: result is 16
  Added 16 and 7: result is 23
-}
