-- file: modules.hs
-- author: Jacob Xie
-- date: 2023/07/12 09:02:04 Wednesday
-- brief:

import Data.List

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub
