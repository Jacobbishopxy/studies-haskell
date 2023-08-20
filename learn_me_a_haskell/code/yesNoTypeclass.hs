-- file: yesNoTypeclass.hs
-- author: Jacob Xie
-- date: 2023/07/17 08:58:12 Monday
-- brief:

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

--
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True

--
data TrafficLight = Red | Yellow | Green

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

--
yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult
