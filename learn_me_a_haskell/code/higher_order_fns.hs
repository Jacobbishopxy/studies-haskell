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
