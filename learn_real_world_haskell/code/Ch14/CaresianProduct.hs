{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

-- file: CaresianProduct.hs
-- author: Jacob Xie
-- date: 2024/05/01 12:16:07 Wednesday
-- brief:

module Main where

comprehensive :: [a] -> [b] -> [(a, b)]
comprehensive xs ys = [(x, y) | x <- xs, y <- ys]

monadic :: (Monad m) => m a -> m b -> m (a, b)
monadic xs ys = do x <- xs; y <- ys; return (x, y)

blockyDo :: (Monad m) => m a -> m b -> m (a, b)
blockyDo xs ys = do
  x <- xs
  y <- ys
  return (x, y)

blockyPlain :: (Monad m) => m a -> m b -> m (a, b)
blockyPlain xs ys =
  xs
    >>= \x ->
      ys
        >>= \y -> return (x, y)

blockyPlain_reloaded :: [a] -> [b] -> [(a, b)]
blockyPlain_reloaded xs ys =
  concatMap (\x -> concatMap (\y -> return (x, y)) ys) xs

main :: IO ()
main = do
  putStrLn "whatever"
