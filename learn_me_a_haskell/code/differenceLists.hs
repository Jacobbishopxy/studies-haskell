-- file: differenceLists.hs
-- author: Jacob Xie
-- date: 2023/08/12 19:32:48 Saturday
-- brief:

import Control.Monad.Trans.Writer

newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

instance Semigroup (DiffList a) where
  -- DiffList f <> DiffList g = DiffList (\xs -> f (g xs))
  DiffList f <> DiffList g = DiffList (f . g)

instance Monoid (DiffList a) where
  -- (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))
  -- (DiffList f) `mappend` (DiffList g) = DiffList (f . g)
  -- mappend = (<>)

  -- mempty = DiffList (\xs -> [] ++ xs)
  mempty = DiffList ([] ++)

gcd' :: Int -> Int -> Writer (DiffList String) Int
gcd' a b
  | b == 0 = do
      tell (toDiffList ["Finished with " ++ show a])
      return a
  | otherwise = do
      result <- gcd' b (a `mod` b)
      tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])
      return result

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
  tell (toDiffList ["0"])
finalCountDown x = do
  finalCountDown (x - 1)
  tell (toDiffList [show x])

main :: IO ()
main = do
  putStrLn $ "test DiffList: " ++ show (fromDiffList (toDiffList [1, 2, 3, 4] <> toDiffList [1, 2, 3]))

  let res = runWriter (gcd' 110 34)
  putStrLn $ "test new gcd: " ++ show (fst res)
  -- mapM_ putStrLn . fromDiffList . snd . runWriter $ gcd' 110 34
  mapM_ putStrLn (fromDiffList (snd res))

  mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown 5000
