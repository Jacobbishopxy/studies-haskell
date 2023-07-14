-- file: modules.hs
-- author: Jacob Xie
-- date: 2023/07/12 09:02:04 Wednesday
-- brief:

import Data.List (nub, tails)
import Data.Map qualified as Map

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

search :: (Eq a) => [a] -> [a] -> Bool
-- search needle haystack =
--   let nlen = length needle
--    in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)
search needle haystack =
  let nlen = length needle
   in foldl (\acc x -> (take nlen x == needle) || acc) False (tails haystack)

fromList' :: (Ord k) => [(k, v)] -> Map.Map k v
fromList' = foldr (\(k, v) acc -> Map.insert k v acc) Map.empty

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2)

phoneBookToMap' :: (Ord k) => [(k, a)] -> Map.Map k [a]
phoneBookToMap' = Map.fromListWith (++) . map (\(k, v) -> (k, [v]))
