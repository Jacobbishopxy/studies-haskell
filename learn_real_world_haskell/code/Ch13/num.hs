-- file: num.hs
-- author: Jacob Xie
-- date: 2024/01/07 21:30:19 Sunday
-- brief:

import Data.List

----------------------------------------------------------------------------------------------------
-- Symbolic/units manipulation
----------------------------------------------------------------------------------------------------

data Op
  = Plus
  | Minus
  | Mul
  | Div
  | Pow
  deriving (Eq, Show)

{-
  The core symbolic manipulation type.
  It can be a simple number, a symbol, a binary arithmetic operation (such as +),
  or a unary arithmetic operation (such as cos)

  Notice the types of BinaryArith and UnaryArith: it's a recursive type.
  So, we could represent a (+) over two SymbolicManips.
-}
data SymbolicManips a
  = Number a -- Simple number, such as 5
  | Symbol String -- A symbol, such as x
  | BinaryArith Op (SymbolicManips a) (SymbolicManips a)
  | UnaryArith String (SymbolicManips a)
  deriving (Eq)

{-
  SymbolicManips will be an instance of Num.
  Define how the Num operations are handled over a SymbolicManips.
  This will implement things like (+) for SymbolicManips.
-}
instance (Num a) => Num (SymbolicManips a) where
  a + b = BinaryArith Plus a b
  a - b = BinaryArith Minus a b
  a * b = BinaryArith Mul a b
  negate = BinaryArith Mul (Number (-1))
  abs = UnaryArith "abs"
  signum _ = error "signum is unimplemented"
  fromInteger = Number . fromInteger

{-
  Make SymbolicManips an instance of Fractional
-}
instance (Fractional a) => Fractional (SymbolicManips a) where
  a / b = BinaryArith Div a b
  recip = BinaryArith Div $ Number 1
  fromRational = Number . fromRational

{-
  Make SymbolicManips an instance of Floating
-}
instance (Floating a) => Floating (SymbolicManips a) where
  pi = Symbol "pi"
  exp = UnaryArith "exp"
  log = UnaryArith "log"
  sqrt = UnaryArith "sqrt"
  a ** b = BinaryArith Pow a b
  sin = UnaryArith "sin"
  cos = UnaryArith "cos"
  tan = UnaryArith "tan"
  asin = UnaryArith "asin"
  acos = UnaryArith "acos"
  atan = UnaryArith "atan"
  sinh = UnaryArith "sinh"
  cosh = UnaryArith "cosh"
  tanh = UnaryArith "tanh"
  asinh = UnaryArith "asinh"
  acosh = UnaryArith "acosh"
  atanh = UnaryArith "atanh"

{-
  Show a SymbolicManips as a String, using conventional algebraic notation
-}
prettyShow :: (Show a, Num a) => SymbolicManips a -> String
-- Show a number or symbol as a bare number or serial
prettyShow (Number x) = show x
prettyShow (Symbol x) = x
prettyShow (BinaryArith op a b) =
  let pa = simpleParen a
      pb = simpleParen b
      pop = op2str op
   in pa ++ pop ++ pb
prettyShow (UnaryArith opstr a) =
  opstr ++ "(" ++ prettyShow a ++ ")"

op2str :: Op -> String
op2str Plus = "+"
op2str Minus = "-"
op2str Mul = "*"
op2str Div = "/"
op2str Pow = "**"

{-
  Add parenthesis where needed. This function is fairly conservative and will
  add parenthesis when not needed in some cases.

  Haskell will have already figured out precedence for us while building up
  the SymbolicManips.
-}
simpleParen :: (Show a, Num a) => SymbolicManips a -> String
simpleParen (Number x) = prettyShow (Number x)
simpleParen (Symbol x) = prettyShow (Symbol x)
simpleParen x@(BinaryArith {}) = "(" ++ prettyShow x ++ ")"
simpleParen x@(UnaryArith {}) = prettyShow x

{-
  Showing a SymbolicManips calls the prettyShow function on it
-}
instance (Show a, Num a) => Show (SymbolicManips a) where
  show = prettyShow

{-
  Show a SymbolicManip using RPN. HP calculator users may find this familiar.
-}
rnpShow :: (Show a, Num a) => SymbolicManips a -> String
rnpShow i =
  let toList (Number x) = [show x]
      toList (Symbol x) = [x]
      toList (BinaryArith op a b) = toList a ++ toList b ++ [op2str op]
      toList (UnaryArith op a) = toList a ++ [op]
      join :: [a] -> [[a]] -> [a]
      join = intercalate
   in join " " (toList i)

{-
  Perform some basic algebraic simplifications on a SymbolicManip
-}
simplify :: (Num a, Eq a) => SymbolicManips a -> SymbolicManips a
simplify (BinaryArith op ia ib) =
  let sa = simplify ia
      sb = simplify ib
   in case (op, sa, sb) of
        (Mul, Number 1, b) -> b
        (Mul, a, Number 1) -> a
        (Mul, Number 0, _) -> Number 0
        (Mul, _, Number 0) -> Number 0
        (Div, a, Number 1) -> a
        (Plus, a, Number 0) -> a
        (Plus, Number 0, b) -> b
        (Minus, a, Number 0) -> a
        _ -> BinaryArith op sa sb
simplify (UnaryArith op a) = UnaryArith op (simplify a)
simplify x = x

{-
  New data type: Units.
  A Units type contains a number and a SymbolicManip, which represents the units of measure.
  A simple label would be something like (Symbol "m")
-}
data Units a = Units a (SymbolicManips a) deriving (Eq)

{-
  Implement Units for Num.
  We don't know how to convert between arbitrary units, so we generate an error if we try to add
  numbers with different units. For multiplication, generate the appropriate new units.
-}
instance (Num a, Eq a) => Num (Units a) where
  (Units xa ua) + (Units xb ub)
    | ua == ub = Units (xa + xb) ua
    | otherwise = error "Mis-matched units in add or subtract"
  (Units xa ua) - (Units xb ub) = Units xa ua + Units (xb * (-1)) ub
  (Units xa ua) * (Units xb ub) = Units (xa * xb) (ua * ub)
  negate (Units xa ua) = Units (negate xa) ua
  abs (Units xa ua) = Units (abs xa) ua
  signum (Units xa _) = Units (signum xa) (Number 1)
  fromInteger i = Units (fromInteger i) (Number 1)

{-
  Make Units an instance of Fractional
-}
instance (Fractional a, Eq a) => Fractional (Units a) where
  (Units xa ua) / (Units xb ub) = Units (xa / xb) (ua / ub)
  recip a = 1 / a
  fromRational r = Units (fromRational r) (Number 1)

{-
  Floating implementation for Units.
  Use some intelligence for angle calculations: support deg and rad
-}
instance (Floating a, Eq a) => Floating (Units a) where
  pi = Units pi $ Number 1
  exp = error "exp not yet implemented in Units"
  log = error "log not yet implemented in Units"
  sqrt (Units xa ua) = Units (sqrt xa) (sqrt ua)
  (Units xa ua) ** (Units xb ub)
    | ub == Number 1 = Units (xa ** xb) (ua ** Number xb)
    | otherwise = error "units for RHS of ** not supported"
  logBase _ = error "logBase not yet implemented in Units"
  sin (Units xa ua)
    | ua == Symbol "rad" = Units (sin xa) (Number 1)
    | ua == Symbol "deg" = Units (sin (deg2rad xa)) (Number 1)
    | otherwise = error "Units for sin must be empty"
  cos (Units xa ua)
    | ua == Symbol "rad" = Units (cos xa) (Number 1)
    | ua == Symbol "deg" = Units (cos (deg2rad xa)) (Number 1)
    | otherwise = error "Units for cos must be empty"
  tan (Units xa ua)
    | ua == Symbol "rad" = Units (tan xa) (Number 1)
    | ua == Symbol "deg" = Units (tan (deg2rad xa)) (Number 1)
    | otherwise = error "Units for tan must be empty"
  asin (Units xa ua)
    | ua == Number 1 = Units (rad2deg $ asin xa) (Symbol "deg")
    | otherwise = error "Units for asin must be empty"
  acos (Units xa ua)
    | ua == Number 1 = Units (rad2deg $ acos xa) (Symbol "deg")
    | otherwise = error "Units for acos must be empty"
  atan (Units xa ua)
    | ua == Number 1 = Units (rad2deg $ atan xa) (Symbol "deg")
    | otherwise = error "Units for atan must be empty"
  sinh = error "sinh not yet implemented in Units"
  cosh = error "cosh not yet implemented in Units"
  tanh = error "tanh not yet implemented in Units"
  asinh = error "asinh not yet implemented in Units"
  acosh = error "acosh not yet implemented in Units"
  atanh = error "atanh not yet implemented in Units"

{-
  A simple function that takes a number and a String and returns an appropriate Units type to
  represent the number and its unit of measure
-}
units :: (Num z) => z -> String -> Units z
units a b = Units a $ Symbol b

{-
  Extract the number only out of a Units type
-}
dropUnits :: (Num z) => Units z -> z
dropUnits (Units x _) = x

{-
  Utilities for the Unit implementation
-}
deg2rad x = 2 * pi * x / 360

rad2deg x = 360 * x / (2 * pi)

{-
  Showing units: we show the numeric component, an underscore, then the prettyShow version of the simplified units
-}
instance (Show a, Num a, Eq a) => Show (Units a) where
  show (Units xa ua) = show xa ++ "_" ++ prettyShow (simplify ua)

test :: (Num a) => a
test = 2 * 5 + 3
