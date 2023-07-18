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

-- data Frank a b = Frank {FrankField :: b a} deriving (Show)
data Frank a b where
  Frank :: {frankField :: b a} -> Frank a b
  deriving (Show)

instance Tofu Frank where
  tofu = Frank

--
data Barry t k p = Barry {yabba :: p, dabba :: t k}

instance Functor (Barry a b) where
  fmap f (Barry {yabba = x, dabba = y}) = Barry {yabba = f x, dabba = y}
