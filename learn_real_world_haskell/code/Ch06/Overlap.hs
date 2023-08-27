-- file: Overlap.hs
-- author: Jacob Xie
-- date: 2023/08/27 08:11:26 Sunday
-- brief:

class Borked a where
  bork :: a -> String

instance Borked Int where
  bork = show

instance Borked (Int, Int) where
  bork (a, b) = bork a ++ ", " ++ bork b

instance (Borked a, Borked b) => Borked (a, b) where
  bork (a, b) = ">>" ++ bork a ++ " " ++ bork b ++ "<<"

main :: IO ()
main = do
  -- putStrLn $ "bork never succeed: " ++ show ( bork (2,3))

  putStrLn "the line above never compile due to overlapping issue"
