main = do
  putStrLn "Hello, everybody!"
  putStrLn ("Please look at my favorite odd number: " ++ show (filter odd [10 .. 20]))
