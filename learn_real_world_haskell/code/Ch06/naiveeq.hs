-- file: naiveeq.hs
-- author: Jacob Xie
-- date: 2023/08/25 16:43:50 Friday
-- brief:

data Color = Red | Green | Blue

colorEq :: Color -> Color -> Bool
colorEq Red Red = True
colorEq Green Green = True
colorEq Blue Blue = True
colorEq _ _ = False

stringEq :: [Char] -> [Char] -> Bool
stringEq [] [] = True
stringEq (x : xs) (y : ys) = x == y && stringEq xs ys
stringEq _ _ = False

class BasicEq a where
  isEqual :: a -> a -> Bool

instance BasicEq Bool where
  isEqual True True = True
  isEqual False False = True
  isEqual _ _ = False

class BasicEq2 a where
  isEqual2 :: a -> a -> Bool
  isNotEqual2 :: a -> a -> Bool

class BasicEq3 a where
  isEqual3 :: a -> a -> Bool
  isEqual3 x y = not (isNotEqual3 x y)

  isNotEqual3 :: a -> a -> Bool
  isNotEqual3 x y = not (isEqual3 x y)

instance BasicEq3 Color where
  isEqual3 Red Red = True
  isEqual3 Green Green = True
  isEqual3 Blue Blue = True
  isEqual3 _ _ = False

instance Show Color where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"

instance Read Color where
  -- readsPrec is the main function for parsing input
  readsPrec _ value =
    -- We pass tryParse a list of pairs. Each pair has a string
    -- and the desired return value. tryParse will try to match
    -- the input to one of these strings
    tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
    where
      -- If there is nothing left to try, fail
      tryParse [] = []
      tryParse ((attempt, result) : xs) =
        -- Compare the start of the string to be parsed to the
        -- text we are looking for.
        if take (length attempt) value == attempt
          then -- If we have a match, return the result and the remaining input
            [(result, drop (length attempt) value)]
          else -- If we don't have a match, try the next pair in the list of attempts.
            tryParse xs
