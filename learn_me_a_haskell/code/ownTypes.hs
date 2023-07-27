-- file: ownTypes.hs
-- author: Jacob Xie
-- date: 2023/07/15 15:46:47 Saturday
-- brief:

module Shapes
  ( Point (..),
    Shape (..),
    surface,
    nudge,
    baseCircle,
    baseRect,
  )
where

-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

-- surface :: Shape -> Float
-- surface (Circle _ _ r) = pi * r ^ 2
-- surface (Rectangle x1 y1 x2 y2) = abs (x2 - x1) * abs (y2 - y1)

data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) =
  abs (x2 - x1) * abs (y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b =
  Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
-- baseRect = (Rectangle (Point 0 0) .) . Point
baseRect width height = Rectangle (Point 0 0) (Point width height)

--
-- data Person = Person String String Int Float String String deriving (Show)

-- firstName :: Person -> String
-- firstName (Person firstName _ _ _ _ _) = firstName

-- lastName :: Person -> String
-- lastName (Person _ lastname _ _ _ _) = lastname

-- age :: Person -> Int
-- age (Person _ _ age _ _ _) = age

-- height :: Person -> Float
-- height (Person _ _ _ height _ _) = height

-- phoneNumber :: Person -> String
-- phoneNumber (Person _ _ _ _ number _) = number

-- flavor :: Person -> String
-- flavor (Person _ _ _ _ _ flavor) = flavor

--
data Person = Person
  { firstName :: String,
    lastName :: String,
    age :: Int,
    height :: Float,
    phoneNumber :: String,
    flavor :: String
  }
  deriving (Show)

--
-- data Car = Car String String Int deriving (Show)

data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

--
data Vector a = Vector a a a deriving (Show)

vPlus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vPlus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

vMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vMult` m = Vector (i * m) (j * m) (k * m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = i * l + j * m + k * n

-- Derived instances

data Persona = Persona
  { pFirstName :: String,
    pLastName :: String,
    pAge :: Int
  }
  deriving (Eq)

--
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

--
-- type PhoneBook = [(String, String)]

type PhoneNumber = String

type Name = String

type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pNumber pBook = (name, pNumber) `elem` pBook
