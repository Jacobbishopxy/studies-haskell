-- file: chessKnight.hs
-- author: Jacob Xie
-- date: 2023/08/07 22:25:09 Monday
-- brief:

import Control.Monad (guard)

type KnightPos = (Int, Int)

-- moveKnight :: KnightPos -> [KnightPos]
-- moveKnight (c, r) = do
--   (c', r') <-
--     [ (c + 2, r - 1),
--       (c + 2, r + 1),
--       (c - 2, r - 1),
--       (c - 2, r + 1),
--       (c + 1, r - 2),
--       (c + 1, r + 2),
--       (c - 1, r - 2),
--       (c - 1, r + 2)
--       ]
--   guard (c' `elem` [1 .. 8] && r' `elem` [1 .. 8])
--   return (c', r')

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) =
  filter
    onBoard
    [ (c + 2, r - 1),
      (c + 2, r + 1),
      (c - 2, r - 1),
      (c - 2, r + 1),
      (c + 1, r - 2),
      (c + 1, r + 2),
      (c - 1, r - 2),
      (c - 1, r + 2)
    ]
  where
    onBoard (c, r) = c `elem` [1 .. 8] && r `elem` [1 .. 8]

in3 :: KnightPos -> [KnightPos]
in3 start = do
  first <- moveKnight start
  second <- moveKnight first
  moveKnight second

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

main :: IO ()
main = do
  putStrLn $ "can reach in 3: " ++ show ((6, 2) `canReachIn3` (6, 1))
  putStrLn $ "can reach in 3: " ++ show ((6, 2) `canReachIn3` (7, 3))
