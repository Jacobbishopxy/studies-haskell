-- file: MultiplyTo.hs
-- author: Jacob Xie
-- date: 2024/05/01 22:37:24 Wednesday
-- brief:

module MultiplyTo where

guarded :: Bool -> [a] -> [a]
guarded True xs = xs
guarded False _ = []

multiplyTo :: Int -> [(Int, Int)]
multiplyTo n = do
  x <- [1 .. n]
  y <- [x .. n]
  guarded (x * y == n) $ return (x, y)

main :: IO ()
main = do
  putStrLn "whatever"
