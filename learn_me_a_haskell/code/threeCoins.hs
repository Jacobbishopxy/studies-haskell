-- file: threeCoins.hs
-- author: Jacob Xie
-- date: 2023/07/26 21:29:52 Wednesday
-- brief:

import System.Random

-- adding `:: (Bool, StdGen)` for pasting to GHCI, otherwise throw ambiguous type error
-- threeCoins :: StdGen -> (Bool, Bool, Bool)
-- threeCoins gen =
--   let (firstCoin, newGen) = random gen :: (Bool, StdGen)
--       (secondCoin, newGen') = random newGen :: (Bool, StdGen)
--       (thirdCoin, newGen'') = random newGen' :: (Bool, StdGen)
--    in (firstCoin, secondCoin, thirdCoin)

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
  let (firstCoin, newGen) = random gen
      (secondCoin, newGen') = random newGen
      (thirdCoin, newGen'') = random newGen'
   in (firstCoin, secondCoin, thirdCoin)

boolToString :: Bool -> String
boolToString b
  | b = "True"
  | otherwise = "False"

main = do
  let (f, s, t) = threeCoins (mkStdGen 21)

  putStrLn ("firstCoin: " ++ boolToString f)
  putStrLn ("secondCoin: " ++ boolToString s)
  putStrLn ("thirdCoin: " ++ boolToString t)
