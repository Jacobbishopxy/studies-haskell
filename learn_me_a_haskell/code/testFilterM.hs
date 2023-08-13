-- file: testFilterM.hs
-- author: Jacob Xie
-- date: 2023/08/13 22:00:48 Sunday
-- brief:

import Control.Monad
import Control.Monad.Trans.Writer

keepSmall :: Int -> Writer [String] Bool
keepSmall x
  | x < 4 = do
      tell ["Keeping " ++ show x]
      return True
  | otherwise = do
      tell [show x ++ " is tool large, throwing it away"]
      return False

powerset :: [a] -> [[a]]
-- powerset xs = filterM (\x -> [True, False]) xs
powerset = filterM (const [True, False])

main :: IO ()
main = do
  putStrLn $ "keepSmall: " ++ show (fst $ runWriter $ filterM keepSmall [9, 1, 5, 2, 10, 3])

  mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9, 1, 5, 2, 10, 3]

  putStrLn $ "powerset: " ++ show (powerset [1, 2, 3])
