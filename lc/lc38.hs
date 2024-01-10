-- file: lc38.hs
-- author: Jacob Xie
-- date: 2024/01/10 21:25:15 Wednesday
-- brief: LeetCode 38

countAndSay :: Int -> [Int]
countAndSay 1 = [1]
countAndSay i = f i [1]
  where
    f :: Int -> [Int] -> [Int]
    f i xs
      | i == 1 = xs
      | otherwise = f (i - 1) (g 1 xs)
    g :: Int -> [Int] -> [Int]
    g _ [] = []
    g i [x] = [i, x]
    g i (x : x' : xs)
      | x == x' = g (i + 1) (x' : xs)
      | otherwise = i : x : g 1 (x' : xs)
