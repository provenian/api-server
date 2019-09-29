{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module Web.Presenters where

import Data.Aeson
import Data.Aeson.Types (Zero)
import Data.Aeson.Casing
import GHC.Generics (Generic, Rep)

newtype SnakeCase a = SnakeCase a
  deriving (Eq, Show)

instance (ToJSON a, Generic a, GToJSON Zero (Rep a)) => ToJSON (SnakeCase a) where
  toJSON (SnakeCase p) = genericToJSON (aesonDrop 0 snakeCase) p

instance (FromJSON a, Generic a, GFromJSON Zero (Rep a)) => FromJSON (SnakeCase a) where
  parseJSON = fmap SnakeCase . genericParseJSON (aesonDrop 0 snakeCase)
