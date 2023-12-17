-- file: lookup.hs
-- author: Jacob Xie
-- date: 2023/12/17 17:15:11 Sunday
-- brief:

myLookup :: (Eq a) => a -> [(a, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup key ((thisKey, thisVal) : rest) =
  if key == thisKey
    then Just thisVal
    else myLookup key rest
