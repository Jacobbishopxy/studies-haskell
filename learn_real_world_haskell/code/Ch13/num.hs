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
simpleParen x@(BinaryArith _ _ _) = "(" ++ prettyShow x ++ ")"
simpleParen x@(UnaryArith _ _) = prettyShow x

{-
  Showing a SymbolicManips calls the prettyShow function on it
-}
instance (Show a, Num a) => Show (SymbolicManips a) where
  show = prettyShow
