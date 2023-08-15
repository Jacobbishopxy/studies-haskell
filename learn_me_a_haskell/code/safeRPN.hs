-- file: safeRPN.hs
-- author: Jacob Xie
-- date: 2023/08/14 23:12:31 Monday
-- brief:

import Control.Monad
import Data.List

-- solveRPN :: String -> Double
-- solveRPN = head . foldl foldingFunction [] . words

-- foldingFunction :: [Double] -> String -> [Double]
-- foldingFunction (x : y : ys) "*" = (x * y) : ys
-- foldingFunction (x : y : ys) "+" = (x + y) : ys
-- foldingFunction (x : y : ys) "-" = (y - x) : ys
-- foldingFunction xs numberString = read numberString : xs

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of
  [(x, "")] -> Just x
  _ -> Nothing

foldingFunction :: [Double] -> String -> Maybe [Double]
foldingFunction (x : y : ys) "*" = return ((x * y) : ys)
foldingFunction (x : y : ys) "+" = return ((x + y) : ys)
foldingFunction (x : y : ys) "-" = return ((y - x) : ys)
foldingFunction xs numberString = liftM (: xs) (readMaybe numberString)

solveRPN :: String -> Maybe Double
solveRPN st = do
  [result] <- foldM foldingFunction [] (words st)
  return result

main :: IO ()
main = do
  putStrLn $ "readMaybe [1]: " ++ show (readMaybe "1" :: Maybe Int)
  putStrLn $ "readMaybe [2]: " ++ show (readMaybe "GO TO HELL" :: Maybe Int)

  putStrLn $ "foldingFunction [1]: " ++ show (foldingFunction [3, 2] "*")
  putStrLn $ "foldingFunction [2]: " ++ show (foldingFunction [3, 2] "-")
  putStrLn $ "foldingFunction [3]: " ++ show (foldingFunction [] "*")
  putStrLn $ "foldingFunction [4]: " ++ show (foldingFunction [] "1")
  putStrLn $ "foldingFunction [5]: " ++ show (foldingFunction [] "1 wawawawa")

  putStrLn $ "solveRPN [1]: " ++ show (solveRPN "1 2 * 4 +")
  putStrLn $ "solveRPN [2]: " ++ show (solveRPN "1 2 * 4 + 5 *")
  putStrLn $ "solveRPN [2]: " ++ show (solveRPN "1 2 * 4")
  putStrLn $ "solveRPN [2]: " ++ show (solveRPN "1 8 wharglbllargh")
