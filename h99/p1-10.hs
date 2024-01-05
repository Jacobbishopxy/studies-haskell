-- file: p1-10.hs
-- author: Jacob Xie
-- date: 2024/01/04 09:05:05 Thursday
-- brief:

-- P1: Find the last element of a list
myLast :: [a] -> a
myLast [x] = x
myLast (_ : xs) = myLast xs

-- P2: Find the last-but-one (or second-last) element of a list
myButLast :: [a] -> a
myButLast [x, _] = x
myButLast (_ : xs) = myButLast xs

-- P3: Find the K'th element of a list
elementAt :: [a] -> Integer -> a
elementAt (x : xs) i
  | i == 1 = x
  | otherwise = elementAt xs (i - 1)

elementAt' :: [a] -> Integer -> Maybe a
elementAt' [] _ = Nothing
elementAt' (x : xs) i
  | i == 1 = Just x
  | otherwise = elementAt' xs (i - 1)

-- P4: Find the number of elements in a list
myLength :: [a] -> Integer
myLength [] = 0
myLength xs = f xs 0
  where
    f :: [a] -> Integer -> Integer
    f [] i = i
    f (_ : xs) i = f xs (i + 1)

myLength' :: [a] -> Integer
myLength' [] = 0
myLength' (_ : xs) = myLength' xs + 1

-- P5: Reverse a list
myReverse :: [a] -> [a]
myReverse xs = f xs []
  where
    f [] xs = xs
    f (x : xs) ys = f xs (x : ys)

-- P6: Find out whether a list is a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
-- isPalindrome xs = f xs (myReverse xs)
--   where
--     f [] _ = True
--     f (x : xs) (y : ys)
--       | x == y = f xs ys
--       | otherwise = False
isPalindrome xs = xs == myReverse xs

-- P7: Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]

myFlatten :: NestedList a -> [a]
myFlatten a = myReverse $ f a []
  where
    f :: NestedList a -> [a] -> [a]
    f a xs = case a of
      Elem e -> e : xs
      List [] -> xs
      List (y : ys) -> f (List ys) (f y xs)

myFlatten' :: NestedList a -> [a]
myFlatten' a = case a of
  Elem e -> [e]
  List [] -> []
  List (x : xs) -> myFlatten' x ++ myFlatten' (List xs)

-- P8: Eliminate consecutive duplicates of list elements
myCompress :: (Eq a) => [a] -> [a]
myCompress [] = []
myCompress [x] = [x]
myCompress (x : y : xs)
  | x == y = myCompress (y : xs)
  | otherwise = x : myCompress (y : xs)

-- P9: Pack consecutive duplicates of list elements into sublists
myPack :: (Eq a) => [a] -> [[a]]
myPack [] = []
myPack xs = f xs []
  where
    -- processing, working on, result
    f :: (Eq a) => [a] -> [a] -> [[a]]
    f [] xs = [xs]
    f (x : xs) [] = f xs [x]
    f (x : xs) y'@(y : _)
      | x == y = f xs (x : y')
      | otherwise = y' : f xs [x]

-- P10: Run-length encoding of a list
myEncode :: (Eq a) => [a] -> [(Integer, a)]
myEncode [] = []
myEncode (x : xs) = f xs (1, x)
  where
    f :: (Eq a) => [a] -> (Integer, a) -> [(Integer, a)]
    f [] z = [z]
    f (x : xs) (i, y)
      | x == y = f xs (i + 1, x)
      | otherwise = (i, y) : f xs (1, x)
