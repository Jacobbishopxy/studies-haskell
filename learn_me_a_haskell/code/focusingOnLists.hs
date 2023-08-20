-- file: focusingOnLists.hs
-- author: Jacob Xie
-- date: 2023/08/20 00:01:37 Sunday
-- brief:

data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

type ListZipper a = ([a], [a])

goForward :: ListZipper a -> ListZipper a
goForward (x : xs, bs) = (xs, x : bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b : bs) = (b : xs, bs)

main :: IO ()
main = do
  let xs = [1, 2, 3, 4]

  putStrLn $ "goForward: " ++ show (goForward (xs, []))
  putStrLn $ "goForward: " ++ show (goForward ([2, 3, 4], [1]))
  putStrLn $ "goForward: " ++ show (goForward ([3, 4], [2, 1]))
  putStrLn $ "goBack: " ++ show (goForward ([4], [3, 2, 1]))
