{-# LANGUAGE DeriveGeneric #-}
module Main where

import GHC.Generics
import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import Data.Ord
import JsEncode

main = defaultMain $ testGroup "Js encoding tests" (primitives ++ compositeStructures)

data SingleRecord = SingleRecord { a_field :: String } deriving (Show, Generic)

data EitherD = LeftD { left :: String } | RightD { right :: String } deriving (Show, Generic)

data DoubleProduct = DoubleProduct { first :: String, second :: Bool } deriving (Show, Generic)

data Nested = Nested { n_first :: String, nesting :: Maybe Nested } deriving (Show, Generic)

data List a = Nil | List a (List a) deriving (Show, Generic1, Generic)

instance JsEncode EitherD
instance JsEncode SingleRecord
instance JsEncode DoubleProduct
instance JsEncode Nested
instance JsEncode a => JsEncode (List a)

primitives = 
  [ testCase "true"
    ((encode True) @=? (JsPrim $ JsBoolean True))
  , testCase "false"
      ((encode False) @=? (JsPrim $ JsBoolean False))
  , testCase "string"
      ((encode "hello") @=? (JsPrim $ JsString "hello"))
  , testCase "null"
      ((encode ()) @=? (JsPrim JsNull))
  , testCase "int"
      ((encode (1 :: Int)) @=? (JsPrim $ JsInt 1))
  , testCase "float"
      ((encode (1.0 :: Float)) @=? (JsPrim $ JsFloat 1.0))
  , testCase "list"
      ((encode [1 :: Int,2,3]) @=?
      (JsArray [JsPrim $ JsInt 1, JsPrim $ JsInt 2, JsPrim $ JsInt 3]))

  , testCase "nested list"
      ((encode [[1 :: Int],[2 :: Int,3]]) @=?
      (JsArray [JsArray [JsPrim $ JsInt 1], JsArray [JsPrim $ JsInt 2, JsPrim $ JsInt 3]]))
  ]

compositeStructures = 
  [ testCase "single record"
      ((encode (SingleRecord "a")) @=?
      (JsObject [(JsPrim (JsString "a_field"),JsPrim (JsString "a"))]))

  , testCase "eitherd left"
      ((encode (LeftD "l")) @=?
      (JsObject [(JsPrim (JsString "left"),JsPrim (JsString "l"))]))

  , testCase "eitherd right"
      ((encode (LeftD "r")) @=?
      (JsObject [(JsPrim (JsString "left"),JsPrim (JsString "r"))]))

  , testCase "double"
      ((encode (DoubleProduct "f" True)) @=?
      (JsObject [(JsPrim (JsString "first"),JsPrim (JsString "f"))
                  ,(JsPrim (JsString "second"),JsPrim (JsBoolean True))]))

  , testCase "recursive no selectors"
      ((encode (List (1 :: Int) (List 2 Nil))) @=?
      (JsArray [JsPrim $ JsInt 1, JsArray [JsPrim $ JsInt 2, JsPrim JsNull]]))


  , testCase "nested"
      ((encode (Nested "f" (Just $ Nested "s" Nothing))) @=?
      (JsObject [(JsPrim (JsString "n_first"),JsPrim (JsString "f"))
                ,(JsPrim (JsString "nesting"),
                    (JsObject [(JsPrim (JsString "n_first"),JsPrim (JsString "s")), (JsPrim (JsString "nesting"), JsPrim JsNull)])
                )]))
  ]