-- file: askForNumber.hs
-- author: Jacob Xie
-- date: 2023/07/28 21:58:31 Friday
-- brief:

-- import Control.Monad (when)
import Control.Monad (unless)
import System.Random

-- main = do
--   gen <- getStdGen
--   askForNumber gen

-- askForNumber :: StdGen -> IO ()
-- askForNumber gen = do
--   let (randNumber, newGen) = randomR (1, 10) gen :: (Int, StdGen)
--   putStr "Which number in the range from 1 to 10 am I thinking of? "
--   numberString <- getLine
--   -- when (not $ null numberString) $ do
--   unless (null numberString) $ do
--     let number = read numberString
--     if randNumber == number
--       then putStrLn "You are correct!"
--       else putStrLn $ "Sorry, it was" ++ show randNumber
--     askForNumber newGen

-- main as recursion, use `reads` & default to 0 to avoid crash by invalid input
main = do
  gen <- getStdGen
  let (randNumber, _) = randomR (1, 10) gen :: (Int, StdGen)
  putStr "Which number in the range from 1 to 10 am I thinking of? "
  numberString <- getLine
  unless (null numberString) $ do
    let num = case reads numberString of
          [(num, aStr)] -> do
            putStrLn $ "aStr: " ++ aStr -- `aStr` is nothing
            return num
          _ -> do
            putStrLn "default 0"
            return 0
    number <- num

    -- [(number, aStr)] <- return $ reads numberString
    -- putStrLn aStr

    if randNumber == number
      then putStrLn "You are correct!"
      else putStrLn $ "Sorry, it was " ++ show randNumber
    newStdGen
    main
