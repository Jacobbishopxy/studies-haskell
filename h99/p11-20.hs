-- file: p11-20.hs
-- author: Jacob Xie
-- date: 2024/01/05 19:56:27 Friday
-- brief:

-- P11: Modified run-length encoding
data SMVal a = Single a | Multiple Integer a deriving (Show)

myEncodeModified :: (Eq a) => [a] -> [SMVal a]
myEncodeModified [] = []
myEncodeModified (x : xs) = f xs (1, x)
  where
    f :: (Eq a) => [a] -> (Integer, a) -> [SMVal a]
    f [] (i, a)
      | i == 1 = [Single a]
      | otherwise = [Multiple i a]
    f (x' : xs') (i, y)
      | x' == y = f xs' (i + 1, x')
      | i == 1 = Single y : f xs' (1, x')
      | otherwise = Multiple i y : f xs' (1, x')

-- P12: Decode a run-length encoded list
myDecodeModified :: (Eq a) => [SMVal a] -> [a]
myDecodeModified [] = []
myDecodeModified (x : xs) = case x of
  Single e -> e : myDecodeModified xs
  Multiple i e -> [e | _ <- [1 .. i]] ++ myDecodeModified xs

-- P13: Run-length encoding of a list (direct solution)
-- same as P11

-- P14: Duplicate the elements of a list
myDupli :: (Eq a) => [a] -> [a]
myDupli [] = []
myDupli (x : xs) = x : x : myDupli xs

-- P15: Replicate the elements of a list a given number of times
myRepli :: (Eq a) => [a] -> Integer -> [a]
myRepli [] _ = []
myRepli (x : xs) i = [x | _ <- [1 .. i]] ++ myRepli xs i

-- P16: Drop every N'th element from a list
myDropEvery :: [a] -> Integer -> [a]
myDropEvery [] _ = []
myDropEvery xs i = f xs i 1
  where
    f :: [a] -> Integer -> Integer -> [a]
    f [] _ _ = []
    f (x : xs') i' j
      | i == j = f xs' i' 1
      | otherwise = x : f xs' i' (j + 1)

-- P17: Split a list into two parts; the length of the first part is given
mySplit :: [a] -> Integer -> ([a], [a])
mySplit [] _ = ([], [])
mySplit xs i = f xs i []
  where
    f :: [a] -> Integer -> [a] -> ([a], [a])
    f [] _ ys = ([], ys)
    f (x : xs') i' ys
      | i' == 1 = (ys ++ [x], xs')
      | otherwise = f xs' (i' - 1) (ys ++ [x])

mySplit' :: [a] -> Integer -> ([a], [a])
mySplit' [] _ = ([], [])
mySplit' l@(x : xs) i
  | i > 0 = (x : ys, zs)
  | otherwise = ([], l)
  where
    (ys, zs) = mySplit' xs (i - 1)

-- P18: Extract a slice from a list
mySlice :: [a] -> Integer -> Integer -> [a]
mySlice [] _ _ = []
mySlice xs i j = f xs i j []
  where
    f :: [a] -> Integer -> Integer -> [a] -> [a]
    f [] _ _ _ = []
    f (x : xs') i' j' ys
      | i' == 1 && j' > 1 = f xs' 1 (j' - 1) (ys ++ [x])
      | i' == 1 && j' == 1 = ys ++ [x]
      | otherwise = f xs' (i' - 1) (j' - 1) ys

mySlice' :: [a] -> Integer -> Integer -> [a]
mySlice' [] _ _ = []
mySlice' (x : xs) i j
  | i > 1 = ys
  | j > 0 = x : ys
  | otherwise = error "`i` or `j` is not a valid number"
  where
    ys = mySlice xs (i - 1) (j - 1)

-- P19: Rotate a list N places to the left
-- myRotate ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"
-- myRotate ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"
myRotate :: [a] -> Integer -> [a]
myRotate [] _ = []
myRotate xs i = f xs (absI i) []
  where
    f :: [a] -> Integer -> [a] -> [a]
    f [] _ _ = []
    f l@(x : xs') i' ys
      | i' == 0 = l ++ ys
      | i' > 0 = f xs' (i' - 1) (ys ++ [x])
      | otherwise = error "`i` is a negative number!"
    s :: [a] -> Integer
    s [] = 0
    s (_ : xs') = s xs' + 1
    absI i'
      | i' > 0 = i'
      | otherwise = s xs + i'

-- std fn: `drop` `take` & `length`
myRotate' :: [a] -> Int -> [a]
myRotate' xs n
  | n >= 0 = drop n xs ++ take n xs
  | otherwise = drop l xs ++ take l xs
  where
    l = n + length xs

-- std fn: `tail` `head` `last` & `init`
myRotate'' :: [a] -> Int -> [a]
myRotate'' [] _ = []
myRotate'' x 0 = x
myRotate'' x y
  | y > 0 = myRotate'' (tail x ++ [head x]) (y - 1)
  | otherwise = myRotate'' (last x : init x) (y + 1)

-- P20: Remove the K'th element from a list
myRemoveAt :: Integer -> [a] -> (Maybe a, [a])
myRemoveAt _ [] = (Nothing, [])
myRemoveAt i xs = f i xs []
  where
    f :: Integer -> [a] -> [a] -> (Maybe a, [a])
    f _ [] xs' = (Nothing, xs')
    f i' (x : xs') ys
      | i' > 1 = f (i' - 1) xs' (ys ++ [x])
      | otherwise = (Just x, ys ++ xs')

myRemoveAt' :: Integer -> [a] -> (Maybe a, [a])
myRemoveAt' _ [] = (Nothing, [])
myRemoveAt' 1 (x : xs) = (Just x, xs)
myRemoveAt' i (x : xs) = let (a, r) = myRemoveAt' (i - 1) xs in (a, x : r)
