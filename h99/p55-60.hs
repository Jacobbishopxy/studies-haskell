-- file: p55-60.hs
-- author: Jacob Xie
-- date: 2024/02/04 16:52:45 Sunday
-- brief:

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

-- P55: Construct completely balanced binary trees
myCbalTree :: Int -> [Tree Char]
myCbalTree 0 = [Empty]
myCbalTree n
  -- odd nodes (including top node), where left & right are not empty
  | n `mod` 2 == 1 =
      [ Branch 'x' l r
        | l <- myCbalTree ((n - 1) `div` 2),
          r <- myCbalTree ((n - 1) `div` 2)
      ]
  -- even
  | otherwise =
      concat
        [ [Branch 'x' l r, Branch 'x' r l]
          | l <- myCbalTree ((n - 1) `div` 2),
            r <- myCbalTree (n `div` 2)
        ]
