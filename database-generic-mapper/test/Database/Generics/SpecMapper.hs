{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Generics.SpecMapper where

import qualified Data.Map as M
import Database.Generics.Mapper
import Database.Generics.Mapper.MySQL
import Test.Tasty.Hspec
import GHC.Generics

data Sample = Sample {
  key :: VarChar 20 :- '["PRIMARY KEY"],
  name :: BigInt :- '["NOT NULL"],
  single :: String
} deriving (Eq, Show, Generic)

spec_Sample_record :: Spec
spec_Sample_record = do
  describe "Sample" $ do
    it "should generate recordTypeOf" $ do
      recordTypeOf Sample{}
        `shouldBe` ( "Sample"
                   , M.fromList
                     [ ("key"   , ("varchar(20)", ["PRIMARY KEY"]))
                     , ("name"  , ("bigint", ["NOT NULL"]))
                     , ("single", ("text", []))
                     ]
                   )
    it "should mapToSQLValues" $ do
      mapToSQLValues (Sample (Field $ VarChar "foo") (Field $ BigInt 100) "bar")
        `shouldBe` [ ("key"   , SQLVarChar "foo")
                   , ("name"  , SQLBigInt 100)
                   , ("single", SQLText "bar")
                   ]
    it "should mapFromSQLValues" $ do
      mapFromSQLValues
          Sample{}
          ( M.fromList
            [ ("key"   , SQLVarChar "foo")
            , ("name"  , SQLBigInt 100)
            , ("single", SQLText "bar")
            ]
          )
        `shouldBe` (Sample (Field $ VarChar "foo") (Field $ BigInt 100) "bar")
    it "should mapFromSQLValues with random order" $ do
      mapFromSQLValues
          Sample{}
          ( M.fromList
            [ ("single", SQLText "bar")
            , ("name"  , SQLBigInt 100)
            , ("key"   , SQLVarChar "foo")
            ]
          )
        `shouldBe` (Sample (Field $ VarChar "foo") (Field $ BigInt 100) "bar")
    it "should mapFromSQLValues with default values" $ do
      mapFromSQLValues
          Sample {key = Field $ VarChar "def", name = Field $ BigInt (-1)}
          (M.fromList [("name", SQLBigInt 100), ("single", SQLText "bar")])
        `shouldBe` (Sample (Field $ VarChar "def") (Field $ BigInt 100) "bar")

data ManyFields = ManyFields {
  f1 :: String,
  f2 :: String,
  f3 :: String,
  f4 :: String,
  f5 :: String,
  f6 :: String,
  f7 :: String,
  f8 :: String,
  f9 :: String,
  f10 :: String,
  f11 :: String,
  f12 :: String,
  f13 :: String,
  f14 :: String,
  f15 :: String,
  f16 :: String,
  f17 :: String,
  f18 :: String,
  f19 :: String,
  f20 :: String,
  f21 :: String,
  f22 :: String,
  f23 :: String,
  f24 :: String,
  f25 :: String,
  f26 :: String,
  f27 :: String
} deriving Generic

spec_ManyFields_record :: Spec
spec_ManyFields_record = do
  describe "ManyFields" $ do
    it "should generate createTable query" $ do
      createTable ManyFields{} /= "" `shouldBe` True
