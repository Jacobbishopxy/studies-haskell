-- file: lengthCompare.hs
-- author: Jacob Xie
-- date: 2023/08/05 19:56:01 Saturday
-- brief:

-- Version#1
-- lengthCompare :: String -> String -> Ordering
-- lengthCompare x y =
--   let a = length x `compare` length y
--       b = x `compare` y
--    in if a == EQ then b else a

-- Version#2
-- lengthCompare :: String -> String -> Ordering
-- lengthCompare x y = (length x `compare` length y) `mappend` (x `compare` y)

lengthCompare :: String -> String -> Ordering
lengthCompare x y =
  (length x `compare` length y)
    `mappend` (vowels x `compare` vowels y)
    `mappend` (x `compare` y)
  where
    vowels = length . filter (`elem` "aeiou")
