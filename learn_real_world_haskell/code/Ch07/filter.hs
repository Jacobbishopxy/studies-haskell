-- file: filter.hs
-- author: Jacob Xie
-- date: 2023/08/29 23:12:14 Tuesday
-- brief:

main = interact $ unlines . filter (elem 'a') . lines
