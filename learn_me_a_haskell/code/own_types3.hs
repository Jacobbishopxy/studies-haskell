-- file: own_types3.hs
-- author: Jacob Xie
-- date: 2023/07/17 22:48:45 Monday
-- brief:

--
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

--
class Tofu t where
  tofu :: j a -> t a j
