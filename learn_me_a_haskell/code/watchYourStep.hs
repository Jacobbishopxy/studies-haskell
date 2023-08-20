-- file: watchYourStep.hs
-- author: Jacob Xie
-- date: 2023/08/20 17:06:17 Sunday
-- brief:

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

freeTree :: Tree Char
freeTree =
  Node
    'P'
    ( Node
        'O'
        (Node 'L' (Node 'N' Empty Empty) (Node 'T' Empty Empty))
        (Node 'Y' (Node 'S' Empty Empty) (Node 'A' Empty Empty))
    )
    ( Node
        'L'
        (Node 'W' (Node 'C' Empty Empty) (Node 'R' Empty Empty))
        (Node 'A' (Node 'A' Empty Empty) (Node 'C' Empty Empty))
    )

data Crumb a = LeftCrumb a (Tree a) | RightCrumb a (Tree a) deriving (Show)

type Breadcrumbs' a = [Crumb a]

type Zipper a = (Tree a, Breadcrumbs' a)

x -: f = f x

goLeft :: Zipper a -> Maybe (Zipper a)
goLeft (Node x l r, bs) = Just (l, LeftCrumb x r : bs)
goLeft (Empty, _) = Nothing

goRight :: Zipper a -> Maybe (Zipper a)
goRight (Node x l r, bs) = Just (r, RightCrumb x l : bs)
goRight (Empty, _) = Nothing

goUp :: Zipper a -> Maybe (Zipper a)
goUp (t, LeftCrumb x r : bs) = Just (Node x t r, bs)
goUp (t, RightCrumb x l : bs) = Just (Node x l t, bs)
goUp (_, []) = Nothing

main :: IO ()
main = do
  -- won't work, cuz return `Maybe (Zipper a)`
  -- let newFocus = (freeTree, []) -: goLeft -: goRight

  -- let newFocus = return (freeTree, []) >>= goLeft >>= goRight
  -- Monad law, left identity
  let newFocus = goLeft (freeTree, []) >>= goRight

  putStrLn $ "newFocus: " ++ show newFocus
