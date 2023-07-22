-- file: shortlines.hs
-- author: Jacob Xie
-- date: 2023/07/22 14:19:10 Saturday
-- brief:

-- main = do
--   contents <- getContents
--   putStr $ shortLinesOnly contents

-- main = interact shortLinesOnly

-- shortLinesOnly :: String -> String
-- shortLinesOnly input =
--   let allLines = lines input
--       shortLines = filter (\line -> length line < 10) allLines
--       result = unlines shortLines
--    in result

main = interact $ unlines . filter ((< 10) . length) . lines
