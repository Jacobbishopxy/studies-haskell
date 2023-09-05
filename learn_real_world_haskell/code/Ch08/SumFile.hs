-- file: SumFile.hs
-- author: Jacob Xie
-- date: 2023/09/04 22:16:56 Monday
-- brief:

-- don't use it in production due to its low efficiency
main :: IO ()
main = do
  contents <- getContents
  print $ sumFile contents
  where
    sumFile = sum . map read . words
