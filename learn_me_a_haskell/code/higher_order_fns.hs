-- file: higher_order_fns.hs
-- author: Jacob Xie
-- date: 2023/07/09 22:52:33 Sunday
-- brief:

--
divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10)

--
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

--
filp' :: (a -> b -> c) -> (b -> a -> c)
filp' f = g
  where
    g x y = f y x

--
filp'' :: (a -> b -> c) -> b -> a -> c
filp'' f y x = f x y

--
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

--
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x : xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

--
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort (filter (<= x) xs)
      biggerSorted = quicksort (filter (> x) xs)
   in smallerSorted ++ [x] ++ biggerSorted

--
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999 ..])
  where
    p x = x `mod` 3829 == 0

-- Collatz sequences
chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n = n : chain (n * 3 + 1)

--
numLongChains :: Int
numLongChains = length (filter isLong (map chain [1 .. 100]))
  where
    isLong xs = length xs > 15
