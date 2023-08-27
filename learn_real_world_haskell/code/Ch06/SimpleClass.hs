-- file: SimpleClass.hs
-- author: Jacob Xie
-- date: 2023/08/27 10:13:33 Sunday
-- brief:

-- these are deprecated
-- {-# LANGUAGE OverlappingInstances #-}
-- {-# LANGUAGE TypeSynonymInstances #-}

import Data.List

class Foo a where
  foo :: a -> String

instance {-# OVERLAPS #-} Foo a => Foo [a] where
  -- foo = concat . intersperse ", " . map foo
  foo = intercalate ", " . map foo

instance {-# OVERLAPS #-} Foo Char where
  foo c = [c]

instance {-# OVERLAPS #-} Foo String where
  foo = id

main :: IO ()
main = do
  putStrLn $ "foo: " ++ foo "SimpleClass"
  putStrLn $ "foo: " ++ foo ["a", "b", "c"]
