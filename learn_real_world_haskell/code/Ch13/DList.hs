-- file: DList.hs
-- author: Jacob Xie
-- date: 2024/01/29 21:13:47 Monday
-- brief:

module DList
  ( DList,
    fromList,
    toList,
    empty,
    append,
    cons,
    dfoldr,
  )
where

newtype DList a = DL
  { unDL :: [a] -> [a]
  }

-- NOTE: xs & ys are both not list, their actual type is `[a] -> [a]`
append :: DList a -> DList a -> DList a
append xs ys = DL (unDL xs . unDL ys)

append' :: DList a -> DList a -> DList a
append' (DL xs) (DL ys) = DL (xs . ys)

fromList :: [a] -> DList a
fromList xs = DL (xs ++)

toList :: DList a -> [a]
toList (DL xs) = xs []

empty :: DList a
empty = DL id

-- equivalent of the list type's (:) operator
cons :: a -> DList a -> DList a
cons x (DL xs) = DL $ (x :) . xs

dfoldr :: (a -> b -> b) -> b -> DList a -> b
dfoldr f z xs = foldr f z $ toList xs

safeHead :: DList a -> Maybe a
safeHead xs = case toList xs of
  (y : _) -> Just y
  _ -> Nothing

dmap :: (a -> b) -> DList a -> DList b
dmap f = dfoldr go empty
  where
    go x = cons (f x)

instance Functor DList where
  fmap = dmap

instance Semigroup (DList a) where
  (<>) = append

instance Monoid (DList a) where
  mempty = empty
