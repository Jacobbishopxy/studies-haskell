-- file: read.hs
-- author: Jacob Xie
-- date: 2023/08/26 15:10:38 Saturday
-- brief:

main = do
  putStrLn "Please enter a Double:"
  inpStr <- getLine
  let inpDouble = read inpStr :: Double
  putStrLn $ "Twice" ++ show inpDouble ++ " is " ++ show (inpDouble * 2)
