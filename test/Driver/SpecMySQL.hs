{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Driver.SpecMySQL where

import GHC.Generics (Generic)
import Test.Tasty.Hspec

import Database.Generics.Mapper
import Driver.MySQL

data D = D {
  fooD :: VarChar 64 :- '["PRIMARY KEY"]
} deriving (Eq, Show, Generic)

instance TableMapper D where
  gtableName _ = "EEE"

spec_MySQL :: Spec
spec_MySQL = do
  describe "MySQL Driver" $ do
    it "should createTable" $ do
      createTable D{}
        `shouldBe` "CREATE TABLE IF NOT EXISTS `EEE` (`fooD` varchar(64) PRIMARY KEY)"
