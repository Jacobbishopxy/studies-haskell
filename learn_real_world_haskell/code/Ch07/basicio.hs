-- file: basicio.hs
-- author: Jacob Xie
-- date: 2023/08/30 21:35:41 Wednesday
-- brief:

main :: IO ()
main = do
  putStrLn "Greetings! What is your name?"
  inpStr <- getLine
  putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"
