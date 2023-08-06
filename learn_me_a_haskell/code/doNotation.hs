-- file: doNotation.hs
-- author: Jacob Xie
-- date: 2023/08/06 20:33:48 Sunday
-- brief:

foo :: Maybe String
-- foo = Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
foo = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)

marySue :: Maybe Bool
marySue = do
  x <- Just 9
  Just (x > 8)

justH :: Maybe Char
justH = do
  (x : _) <- Just "hello"
  return x
