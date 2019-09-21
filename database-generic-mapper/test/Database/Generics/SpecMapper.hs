{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Database.Generics.SpecMapper where

import Database.Generics.Mapper
import Test.Tasty.Hspec
import GHC.Generics

data Sample = Sample {
  key :: String :- '["PRIMARY KEY"],
  name :: Int :- '["NOT NULL"],
  single :: String
} deriving Generic

spec_Sample_record :: Spec
spec_Sample_record = do
  describe "Sample" $ do
    it "should generate createTable query" $ do
      createTable Sample{}
        `shouldBe` "CREATE TABLE IF NOT EXISTS `Sample` (`key` text PRIMARY KEY, `name` bigint NOT NULL, `single` text)"
