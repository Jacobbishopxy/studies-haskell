-- file: Arbitraray.hs
-- author: Jacob Xie
-- date: 2023/11/20 22:46:45 Monday
-- brief:

import Control.Monad (liftM, liftM2)
import Prettify2
import Test.QuickCheck

-- this class is provided by `QuickCheck`
-- class Arbitrary a where
--   arbitrary :: Gen a

-- these are key functions from `QuickCheck`
-- elements :: [a] -> Gen a
-- choose :: Random a => (a, a) -> Gen a
-- oneof :: [Gen a ] -> Gen a

data Ternary
  = Yes
  | No
  | Unknown
  deriving (Eq, Show)

-- instance Arbitrary Ternary where
--   arbitrary = elements [Yes, No, Unknown]

instance Arbitrary Ternary where
  arbitrary = do
    n <- choose (0, 2) :: Gen Int
    return $ case n of
      0 -> Yes
      1 -> No
      _ -> Unknown

-- this is provided by `QuickCheck`
-- instance (Arbitrary a, Arbitrary b) => Arbitrary (a, b) where
--   arbitrary = do
--     x <- arbitrary
--     y <- arbitrary
--     return (x, y)

-- newtype for Char, since `Char` has already been implemented `Arbitrary`
newtype DocChar = DocChar Char

fromDocChar :: DocChar -> Char
fromDocChar (DocChar x) = x

instance Arbitrary DocChar where
  arbitrary = elements (DocChar <$> ['A' .. 'Z'] ++ ['a' .. 'z'] ++ " ~!@#$%^&*()")

-- instance Arbitrary Doc where
--   arbitrary = do
--     n <- choose (1, 6) :: Gen Int
--     case n of
--       1 -> return Empty
--       2 -> do
--         (x :: DocChar) <- arbitrary
--         return (Char $ fromDocChar x)
--       3 -> Text <$> arbitrary
--       4 -> return Line
--       5 -> do
--         x <- arbitrary
--         Concat x <$> arbitrary
--       6 -> do
--         x <- arbitrary
--         Union x <$> arbitrary

-- TODO:
-- instance Arbitrary Doc where
--   arbitrary =
--     oneof
--       [ return Empty,
--         liftM DocChar arbitrary,
--         liftM Text arbitrary,
--         return Line,
--         liftM2 Concat arbitrary arbitrary,
--         liftM2 Union arbitrary arbitrary
--       ]
