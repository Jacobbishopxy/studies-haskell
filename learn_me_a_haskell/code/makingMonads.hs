-- file: makingMonads.hs
-- author: Jacob Xie
-- date: 2023/08/16 22:34:43 Wednesday
-- brief:

import Data.Bifunctor qualified as B
import Data.List (all)
import Data.Ratio ((%))

newtype Prob a = Prob {getProb :: [(a, Rational)]} deriving (Show)

instance Functor Prob where
  -- fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs
  fmap f (Prob xs) = Prob $ map (B.first f) xs

flatten :: Prob (Prob a) -> Prob a
-- flatten (Prob xs) = Prob $ concat $ map multAll xs
--   where
--     multAll (Prob innerxs, p) = map (\(x, r) -> (x, p * r)) innerxs
flatten (Prob xs) = Prob $ concatMap multAll xs
  where
    multAll (Prob innerxs, p) = map (B.second (p *)) innerxs

instance Applicative Prob where
  pure x = Prob [(x, 1 % 1)]
  Prob fs <*> Prob xs = Prob $ [(f x, p * q) | (f, p) <- fs, (x, q) <- xs]

instance Monad Prob where
  -- return x = Prob [(x,1%1)]
  m >>= f = flatten (fmap f m)

instance MonadFail Prob where
  fail _ = Prob []

data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads, 1 % 2), (Tails, 1 % 2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 1 % 10), (Tails, 9 % 10)]

flipThree :: Prob Bool
flipThree = do
  a <- coin
  b <- coin
  c <- loadedCoin
  return (all (== Tails) [a, b, c])

getX x
  | x == True = "a"
  | otherwise = "b"

getJoinedProb :: Prob Bool -> (Rational, Rational)
getJoinedProb = foldr (\(x, p) (accT, accF) -> if x then (accT + p, accF) else (accT, accF + p)) (0, 0) . getProb

main :: IO ()
main = do
  putStrLn $ "test Prob: " ++ show (fmap negate (Prob [(3, 1 % 2), (5, 1 % 4), (9, 1 % 4)]))

  putStrLn $ "flipThree: " ++ show (getProb flipThree)

  putStrLn $ "flipThree joined: " ++ show (getJoinedProb flipThree)
