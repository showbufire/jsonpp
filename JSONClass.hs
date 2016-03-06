{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module JSONClass where

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject (JObj JValue)
            | JArray (JAry JValue)
              deriving (Eq, Ord, Show)

type JSONError = String

class JSON a where
    toJValue   :: a -> JValue
    fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
    toJValue = id
    fromJValue = Right

instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = Right b
    fromJValue _         = Left "not a JSON boolean"

instance JSON String where
    toJValue = JString
    fromJValue (JString s) = Right s
    fromJValue _           = Left "not a JSON string"

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)

instance JSON Int where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Integer where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Double where
    toJValue = JNumber
    fromJValue = doubleToJValue id

newtype JAry a = JAry {
      fromJAry :: [a]
    } deriving (Eq, Ord, Show)

jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)
jaryFromJValue (JArray (JAry xs)) = whenRight JAry (mapEithers fromJValue xs)
jaryFromJValue _ = Left "not a JSON Array"

whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right a) = Right (f a)

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
                        Left err -> Left err
                        Right ys -> case f x of
                                      Left err -> Left err
                                      Right y  -> Right (y:ys)
mapEithers _ _  = Right []

listToJValues :: (JSON a) => [a] -> [JValue]
listToJValues = map toJValue

jaryOfJValuesToJValue :: JAry JValue -> JValue
jaryOfJValuesToJValue = JArray

jaryToJValue :: (JSON a) => JAry a -> JValue 
jaryToJValue = JArray . JAry . map toJValue . fromJAry

instance (JSON a) => JSON (JAry a) where
  toJValue = jaryToJValue
  fromJValue = jaryFromJValue

newtype JObj a = JObj {
      fromJObj :: [(String, a)]
    } deriving (Eq, Ord, Show)

instance (JSON a) => JSON (JObj a) where
  toJValue = jobjToJValue
  fromJValue = jobjFromJValue

jobjToJValue :: (JSON a) => JObj a -> JValue
jobjToJValue (JObj xs) = JObject $ JObj $ zip (map fst xs) (map (toJValue . snd) xs)

jobjFromJValue :: (JSON a) => JValue -> Either JSONError (JObj a)
jobjFromJValue (JObject (JObj xs)) = whenRight (\x -> JObj $ zip (map fst xs) x) (mapEithers fromJValue (map snd xs))
jobjFromJValue _ = Left "not a JSON object"
