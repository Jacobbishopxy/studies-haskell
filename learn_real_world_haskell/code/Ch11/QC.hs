-- file: QC.hs
-- author: Jacob Xie
-- date: 2023/11/22 14:16:02 Wednesday
-- brief:
{-# LANGUAGE TemplateHaskell #-}

module QC where

import Arbitrary
import Data.List (intersperse)
import Prettify2
import Test.QuickCheck.All

prop_empty_id x =
  empty <> x == x
    && x <> empty == x

prop_char c = char c == Char c

prop_text s = text s == if null s then Empty else Text s

prop_line = line == Line

prop_double d = double d == text (show d)

-- Using lists as a model

prop_hcat xs = hcat xs == glue xs
  where
    glue [] = empty
    glue (d : ds) = d <> glue ds

prop_punctuate s xs = punctuate s xs == intersperse s xs

prop_punctuate' s xs = punctuate s xs == combine (intersperse s xs)
  where
    combine [] = []
    combine [x] = [x]
    combine (x : Empty : ys) = x : combine ys
    combine (Empty : y : ys) = y : combine ys
    combine (x : y : ys) = x `Concat` y : combine ys

-- Test main, see https://stackoverflow.com/a/42669557/22544285

return []

runTests = $quickCheckAll
