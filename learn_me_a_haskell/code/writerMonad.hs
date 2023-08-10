-- file: writerMonad.hs
-- author: Jacob Xie
-- date: 2023/08/09 23:23:04 Wednesday
-- brief:

import Data.Monoid

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

-- version#1
-- applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
-- applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

-- version#2
-- applyLog :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])
-- applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

-- version#3
applyLog :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)

--
type Food = String

type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whiskey", Sum 99)
addDrink _ = ("beer", Sum 30)

main :: IO ()
main = do
  --
  let res1 = ("beans", Sum 10) `applyLog` addDrink
  putStrLn $ "res1: " ++ show res1

  let res2 = ("jerky", Sum 25) `applyLog` addDrink
  putStrLn $ "res2: " ++ show res2

  let res3 = ("meat", Sum 5) `applyLog` addDrink
  putStrLn $ "res3: " ++ show res3

  let res4 = ("meat", Sum 5) `applyLog` addDrink `applyLog` addDrink
  putStrLn $ "res4: " ++ show res4
