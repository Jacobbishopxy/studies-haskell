-- file: basicio-nodo.hs
-- author: Jacob Xie
-- date: 2023/08/30 21:37:01 Wednesday
-- brief:

main :: IO ()
main =
  putStrLn "Greetings! What is your name?"
    >> getLine
    >>= (\inpStr -> putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!")
