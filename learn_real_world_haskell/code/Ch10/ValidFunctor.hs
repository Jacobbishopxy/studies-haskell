-- file: ValidFunctor.hs
-- author: Jacob Xie
-- date: 2023/11/05 21:23:15 Sunday
-- brief:

data Foo a = Foo a

instance Functor Foo where
  fmap f (Foo a) = Foo (f a)

-- cannot do this since `a` has a type constraint
-- data Eq a => Bar a = Bar a

-- instance Functor Bar where
--   fmap f (Bar a) = Bar (f a)
