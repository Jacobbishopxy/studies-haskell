-- file: Sorting.hs
-- author: Jacob Xie
-- date: 2024/04/29 13:21:04 Monday
-- brief:

module Sorting
  ( sort,
    parSort,
    sillySort,
    force,
    seqSort,
    parSort2,
  )
where

import Control.Parallel (par, pseq)

{-
  Normal form and head normal form

  `seq` function evaluates an expression to head normal form (HNF). It stops once it reaches the outermost
  constructor (the "head"). This is distinct from normal form (NF), in which an expression is completely
  evaluated.
-}

-- divide-and-conquer
sort :: (Ord a) => [a] -> [a]
sort (x : xs) = lesser ++ x : greater
  where
    lesser = sort [y | y <- xs, y < x]
    greater = sort [y | y <- xs, y >= x]
sort _ = []

{-
  Transforming our code into parallel code

  `par`: it evaluates its left argument to weak head normal form, and returns its right.
  `pseq`: it evaluates the expression on the left to WHNF before returning the expression on the right.

  diff: the compiler does not promise to evaluate the left argument of `seq` if it can see that evaluating
    the right argument first would improve performance; the compiler guarantees that `pseq` will evaluate
    its left argument before its right.
-}

parSort :: (Ord a) => [a] -> [a]
parSort (x : xs) = force greater `par` (force lesser `pseq` (lesser ++ x : greater))
  where
    lesser = parSort [y | y <- xs, y < x]
    greater = parSort [y | y <- xs, y >= x]
parSort _ = []

force :: [a] -> ()
force xs = go xs `pseq` ()
  where
    go :: [a] -> Integer
    go (_ : xs') = go xs'
    go [] = 1

{-
  Knowing what to evaluate in parallel
-}

sillySort :: (Ord a) => [a] -> [a]
sillySort (x : xs) = greater `par` (lesser `pseq` (lesser ++ x : greater))
  where
    lesser = sillySort [y | y <- xs, y < x]
    greater = sillySort [y | y <- xs, y >= x]
sillySort _ = []

seqSort :: (Ord a) => [a] -> [a]
-- seqSort = undefined
seqSort (x : xs) = lesser `pseq` (greater `pseq` (lesser ++ x : greater))
  where
    lesser = seqSort [y | y <- xs, y < x]
    greater = seqSort [y | y <- xs, y >= x]
seqSort _ = []

-- tuning for performance
parSort2 :: (Ord a) => Int -> [a] -> [a]
parSort2 d lst@(x : xs)
  | d <= 0 = sort lst
  | otherwise = force greater `par` (force lesser `pseq` (lesser ++ x : greater))
  where
    lesser = parSort2 d' [y | y <- xs, y < x]
    greater = parSort2 d' [y | y <- xs, y >= x]
    d' = d - 1
parSort2 _ _ = []
