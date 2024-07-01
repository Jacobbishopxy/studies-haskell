import Control.Parallel (par)

-- file: Strat.hs
-- author: Jacob Xie
-- date: 2024/06/30 10:42:19 Sunday
-- brief:

type Done = ()

type Strategy a = a -> Done

r0 :: Strategy a
r0 _ = ()

rwhnf :: Strategy a
rwhnf x = x `seq` ()

class NFData a where
  rnf :: Strategy a
  rnf = rwhnf

instance NFData Char

instance NFData Int

instance (NFData a) => NFData (Maybe a) where
  rnf Nothing = ()
  rnf (Just x) = rnf x

{- ... and so on ... -}

-- Separating algorithm from strategy

parList :: Strategy a -> Strategy [a]
parList _ [] = ()
parList strat (x : xs) = strat x `par` (parList strat xs)

parMap :: Strategy b -> (a -> b) -> [a] -> [b]
parMap strat f xs = map f xs `using` parList strat

using :: a -> Strategy a -> a
using x s = s x `seq` x

-- vectorSum' :: (NFData a, Num a) => [a] -> [a] -> [a]
-- vectorSum' = parZipWith rnf (+)
