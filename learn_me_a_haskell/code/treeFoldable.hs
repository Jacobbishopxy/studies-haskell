-- file: treeFoldable.hs
-- author: Jacob Xie
-- date: 2023/08/05 22:43:44 Saturday
-- brief:

import Data.Foldable qualified as F
import Data.Monoid

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance F.Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Node x l r) =
    F.foldMap f l `mappend` f x `mappend` F.foldMap f r

main = do
  let testTree =
        Node
          5
          ( Node
              3
              (Node 1 Empty Empty)
              (Node 6 Empty Empty)
          )
          ( Node
              9
              (Node 8 Empty Empty)
              (Node 10 Empty Empty)
          )

  putStrLn $ "testTree\n" ++ show testTree

  putStrLn $ "foldl (+) 0\n" ++ show (F.foldl (+) 0 testTree)

  putStrLn $ "foldl (*) 1\n" ++ show (F.foldl (*) 1 testTree)

  putStrLn $ "tree has 3\n" ++ show (getAny $ F.foldMap (\x -> Any $ x == 3) testTree)

  putStrLn $ "tree to list\n" ++ show (F.foldMap (\x -> [x]) testTree)
