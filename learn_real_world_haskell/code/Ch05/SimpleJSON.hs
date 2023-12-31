-- file: SimpleJSON.hs
-- author: Jacob Xie
-- date: 2023/08/21 20:58:32 Monday
-- brief:

module SimpleJSON
  ( JValue (..),
    getString,
    getInt,
    getDouble,
    getBool,
    getObject,
    getArray,
    isNull,
  )
where

data JValue
  = JString String
  | JNumber Double
  | JBool Bool
  | JNull
  | JObject [(String, JValue)]
  | JArray [JValue]
  deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _ = Nothing

getInt (JNumber n) = Just n
getInt _ = Nothing

getDouble (JNumber n) = Just n
getDouble _ = Nothing

getBool (JBool n) = Just n
getBool _ = Nothing

getObject (JObject o) = Just o
getObject _ = Nothing

getArray (JArray a) = Just a
getArray _ = Nothing

isNull v = v == JNull

--
