-- file: poleAndBirds.hs
-- author: Jacob Xie
-- date: 2023/08/06 17:57:37 Sunday
-- brief:

type Birds = Int

type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise = Nothing

-- always fail
banana :: Pole -> Maybe Pole
banana _ = Nothing

routine1 :: Maybe Pole
routine1 = do
  -- start <- return (0,0)
  let start = (0, 0)
  first <- landLeft 2 start
  second <- landRight 2 first
  landLeft 1 second

routine2 :: Maybe Pole
routine2 = do
  -- start <- return (0,0)
  let start = (0, 0)
  first <- landLeft 2 start
  Nothing
  second <- landRight 2 first
  landLeft 1 second

main = do
  -- let res = return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2
  let res = landRight 2 (0, 0) >>= landLeft 2 >>= landRight 2

  putStrLn $ "res: " ++ show res

  -- let alwaysFail1 = return (0,0) >>= landLeft 1 >>= banana >>= landRight 1
  let alwaysFail1 = landLeft 1 (0, 0) >>= banana >>= landRight 1

  putStrLn $ "always fail [1]: " ++ show alwaysFail1

  -- let alwaysFail2 = return (0, 0) >>= landLeft 1 >> Nothing >>= landRight 1
  let alwaysFail2 = landLeft 1 (0, 0) >> Nothing >>= landRight 1

  putStrLn $ "always fail [2]: " ++ show alwaysFail2

  putStrLn $ "routine [1]: " ++ show routine1

  putStrLn $ "routine [2]: " ++ show routine2
