-- file: p55-60.hs
-- author: Jacob Xie
-- date: 2024/02/04 16:52:45 Sunday
-- brief:

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

singletonTree :: a -> Tree a
singletonTree x = Branch x Empty Empty

replaceTreeL :: Tree a -> a -> Tree a
replaceTreeL (Branch x _ r) a = Branch x (singletonTree a) r
replaceTreeL Empty a = Branch a Empty Empty

replaceTreeR :: Tree a -> a -> Tree a
replaceTreeR (Branch x l _) a = Branch x l (singletonTree a)
replaceTreeR Empty a = Branch a Empty Empty

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

-- P56: Symmetric binary trees
mySymmetric :: Tree a -> Bool
mySymmetric Empty = True
mySymmetric (Branch _ a b) = isMirror a b

isMirror :: Tree a -> Tree a -> Bool
isMirror (Branch _ al ar) (Branch _ bl br) = isMirror al br && isMirror ar bl
isMirror Empty Empty = True
isMirror _ _ = False

-- P57: Binary search trees
-- test case:
-- mySymmetric . myConstruct $ [5, 3, 18, 1, 4, 12, 21]
myConstruct :: [Int] -> Tree Int
myConstruct [x] = singletonTree x
myConstruct xs = foldl treeAdd Empty xs

treeAdd :: (Ord a) => Tree a -> a -> Tree a
treeAdd Empty x = Branch x Empty Empty
treeAdd t@(Branch y l r) x = case compare x y of
  LT -> Branch y (treeAdd l x) r
  GT -> Branch y l (treeAdd r x)
  EQ -> t
