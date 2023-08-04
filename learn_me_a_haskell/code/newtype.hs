-- file: newtype.hs
-- author: Jacob Xie
-- date: 2023/08/04 21:06:08 Friday
-- brief:

-- fmap a tuple's 1st element
newtype Pair b a = Pair {getPair :: (a, b)}

instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)

--
-- data CoolBool = CoolBool {getCoolBool :: Bool}
newtype CoolBool = CoolBool {getCoolBool :: Bool}

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"
