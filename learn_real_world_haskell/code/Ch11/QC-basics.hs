-- file: QC-basics.hs
-- author: Jacob Xie
-- date: 2023/11/14 20:50:22 Tuesday
--
-- deps installation: `cabal install --lib QuickCheck`;

import Data.List
import Test.QuickCheck

-- a custom sort routine
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort lhs ++ [x] ++ qsort rhs
  where
    lhs = filter (< x) xs
    rhs = filter (>= x) xs

--
prop_idempotent xs = qsort (qsort xs) == qsort xs

-- doesn't work on empty list
prop_minimum xs = head (qsort xs) == minimum xs

prop_minimum' xs = not (null xs) ==> head (qsort xs) == minimum xs

prop_ordered xs = ordered (qsort xs)
  where
    ordered [] = True
    ordered [x] = True
    ordered (x : y : xs) = x <= y && ordered (y : xs)

prop_permutation xs = permutation xs (qsort xs)
  where
    permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_maximum xs = not (null xs) ==> last (qsort xs) == maximum xs

prop_append xs ys =
  not (null xs)
    ==> not (null ys)
    ==> head (qsort (xs ++ ys))
    == min (minimum xs) (minimum ys)
