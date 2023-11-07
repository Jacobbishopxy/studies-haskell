-- file: TypeConstraint.hs
-- author: Jacob Xie
-- date: 2023/11/06 23:19:41 Monday
-- brief:

data (Ord a) => OrdStack a
  = Bottom
  | Item a (OrdStack a)
  deriving (Show)

isIncreasing :: (Ord a) => OrdStack a -> Bool
isIncreasing (Item a rest@(Item b _))
  | a < b = isIncreasing rest
  | otherwise = False
isIncreasing _ = True

push :: (Ord a) => a -> OrdStack a -> OrdStack a
-- push a s = Item a s
push = Item
