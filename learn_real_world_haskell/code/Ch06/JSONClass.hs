-- file: JSONClass.hs
-- author: Jacob Xie
-- date: 2023/08/26 23:52:36 Saturday
-- brief:

module JSONClass (JAry (..)) where

import Control.Arrow (second)

-- import SimpleJSON (JValue (..))

-- redefined ADT
data JValue
  = JString String
  | JNumber Double
  | JBool Bool
  | JNull
  | JObject (JObj JValue) -- was [(String, JValue)]
  | JArray (JAry JValue) -- was [JValue]
  deriving (Eq, Ord, Show)

type JSONError = String

class JSON a where
  toJValue :: a -> JValue
  fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
  toJValue = id
  fromJValue = Right

instance JSON Bool where
  toJValue = JBool
  fromJValue (JBool b) = Right b
  fromJValue _ = Left "not a JSON boolean"

instance JSON String where
  toJValue = JString
  fromJValue (JString s) = Right s
  fromJValue _ = Left "not a JSON string"

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _ = Left "not a JSON number"

instance JSON Int where
  toJValue = JNumber . realToFrac
  fromJValue = doubleToJValue round

instance JSON Integer where
  toJValue = JNumber . realToFrac
  fromJValue = doubleToJValue round

instance JSON Double where
  toJValue = JNumber
  fromJValue = doubleToJValue id

-- Overlapping instances

-- instance (JSON a) => JSON [a] where
--   toJValue = undefined
--   fromJValue = undefined

-- instance (JSON a) => JSON [(String, a)] where
--   toJValue = undefined
--   fromJValue = undefined

-- Using newtype keyword to solve overlapping instances' issue

newtype JAry a = JAry {fromJAry :: [a]}
  deriving (Eq, Ord, Show)

newtype JObj a = JObj {fromJObj :: [(String, a)]}
  deriving (Eq, Ord, Show)

-- JSON Array to JSON Value
jaryToJValue :: (JSON a) => JAry a -> JValue
jaryToJValue = JArray . JAry . map toJValue . fromJAry

-- JSON Array from JSON Value
jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)
jaryFromJValue (JArray (JAry a)) = whenRight JAry (mapEithers fromJValue a)
jaryFromJValue _ = Left "not a JSON array"

-- JAry instance of JSON typeclass
instance (JSON a) => JSON (JAry a) where
  toJValue = jaryToJValue
  fromJValue = jaryFromJValue

listToJValues :: (JSON a) => [a] -> [JValue]
listToJValues = map toJValue

jvaluesToJAry :: [JValue] -> JAry JValue
jvaluesToJAry = JAry

jaryOfJValuesToJValue :: JAry JValue -> JValue
jaryOfJValuesToJValue = JArray

whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right a) = Right (f a)

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x : xs) = case mapEithers f xs of
  Left err -> Left err
  Right ys -> case f x of
    Left err -> Left err
    Right y -> Right (y : ys)
mapEithers _ _ = Right []

-- JObj instance of JSON typeclass
instance (JSON a) => JSON (JObj a) where
  toJValue = JObject . JObj . map (second toJValue) . fromJObj
  fromJValue (JObject (JObj o)) = whenRight JObj (mapEithers unwrap o)
    where
      unwrap (k, v) = whenRight (k,) (fromJValue v)
  fromJValue _ = Left "not a JSON object"

main :: IO ()
main = do
  putStrLn $ "[1] " ++ show (toJValue "True")
  putStrLn $ "[2] " ++ show (toJValue (JAry ["a", "b", "c"]))
  putStrLn $ "[3] " ++ show (toJValue (JAry [True, False, False]))
  putStrLn $ "[4] " ++ show (toJValue (JAry [2 :: Double, 3, 4]))
  putStrLn $ "[5] " ++ show (toJValue (JObj [("a", JNumber 3.0), ("b", toJValue "2"), ("d", toJValue (JAry [2 :: Double, 3, 3]))]))
