-- file: letBindingsInDo.hs
-- author: Jacob Xie
-- date: 2023/07/19 23:03:19 Wednesday
-- brief:

import Data.Char

main = do
  putStrLn "What's your first name?"
  firstName <- getLine
  putStrLn "What's your last name?"
  lastName <- getLine

  let bigFirstName = map toUpper firstName
      bigLastName = map toUpper lastName

  putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"
