{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures, InstanceSigs, DefaultSignatures,FlexibleInstances, TypeSynonymInstances, FlexibleContexts #-}
module JsEncode where

import GHC.Generics
import Data.List

data JsValue = JsPrim JsPrimValue | JsArray [JsValue] | JsObject [(JsValue, JsValue)]
  deriving (Show, Eq)

data JsPrimValue = JsString String | JsBoolean Bool | JsChar Char | JsNull | JsInt Int | JsFloat Float
  deriving (Show, Eq)

jsPrint :: JsValue -> String
jsPrint (JsPrim (JsString s)) = "\"" ++ s ++ "\""
jsPrint (JsPrim JsNull) = "null"
jsPrint (JsPrim (JsBoolean True)) = "true"
jsPrint (JsPrim (JsBoolean False)) = "false"
jsPrint (JsPrim (JsFloat x)) = show x
jsPrint (JsPrim (JsInt x)) = show x
jsPrint (JsPrim (JsChar x)) = "'" ++ [x] ++ "'"
jsPrint (JsArray xs) = "[" ++ concat (intersperse ", " $ map jsPrint xs) ++ "]"
jsPrint (JsObject xs) = "{" ++ concat (intersperse ", " $ map (\(x,y) -> jsPrint x ++ ": "  ++ jsPrint y) xs) ++ "}"

class JsEncode' f where
    encode' :: f p -> JsValue

instance JsEncode' U1 where
  encode' U1 = JsPrim JsNull

instance JsEncode c => JsEncode' (K1 i c) where
  encode' (K1 x) = encode x

instance (JsEncode' f) => JsEncode' (Rec1 f) where
  encode' (Rec1 a) = encode' a

instance (Constructor c, JsEncode' a) => JsEncode' (M1 C c a) where
  encode' s@(M1 a) = encode' a

instance (JsEncode' a) => JsEncode' (M1 D d a) where
  encode' s@(M1 a) = encode' a

instance (Selector s, JsEncode' f) => JsEncode' (M1 S s f) where
  encode' s@(M1 a) = 
    let name = selName s 
    in if name == "" then JsArray [encode' a] else JsObject [(JsPrim (JsString name), encode' a)]

instance (JsEncode' f, JsEncode' g) => JsEncode' (f :+: g) where
  encode' (L1 a) = encode' a
  encode' (R1 a) = encode' a

instance (JsEncode' f, JsEncode' g) => JsEncode' (f :*: g) where
  encode' (a :*: b) = 
    case (encode' a, encode' b) of
      (JsArray  xs, JsArray  ys) -> JsArray  $ xs ++ ys
      (JsObject xs, JsObject ys) -> JsObject $ xs ++ ys

class JsEncode t where
  encode :: t -> JsValue
  default encode :: (Generic t, JsEncode' (Rep t)) => t -> JsValue
  encode x = encode' (from x)

instance JsEncode String where encode = JsPrim . JsString
instance JsEncode ()     where encode = const (JsPrim JsNull)
instance JsEncode Int    where encode = JsPrim .  JsInt
instance JsEncode Bool   where encode = JsPrim . JsBoolean
instance JsEncode Float  where encode = JsPrim . JsFloat
instance JsEncode a => JsEncode (Maybe a) where
  encode = maybe (JsPrim $ JsNull) encode 

instance {-# OVERLAPPABLE #-} JsEncode a => JsEncode [a] where
  encode x = JsArray $ fmap encode x


-- Example!

data Basket = Basket { items :: [Item] } deriving (Generic, Show)

data Item = Item {
  name :: String,
  pid :: String,
  value :: Int,
  quantity :: Int,
  service :: Maybe Service
} deriving (Generic, Show)

data Service = Service { deliver :: Bool, extraFast :: Bool } deriving (Generic, Show)

basket = Basket { items = [
  Item { name = "test", pid = "123", quantity = 1, value = 10, service = Nothing },
  Item { name = "test", pid = "123", quantity = 1, value = 10, service = Just $ Service { deliver = False, extraFast = True } }
]}

instance JsEncode Basket 
instance JsEncode Item
instance JsEncode Service

jsonBasket = putStrLn $ jsPrint $ encode basket
