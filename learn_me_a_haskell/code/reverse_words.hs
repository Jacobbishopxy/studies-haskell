-- file: reverse_words.hs
-- author: Jacob Xie
-- date: 2023/07/19 23:46:56 Wednesday
-- brief:

main = do
  putStrLn "Start your words, exit by enter"
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn $ reverseWords line
      main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
