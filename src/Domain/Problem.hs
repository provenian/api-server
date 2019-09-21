module Domain.Problem where

import Data.Aeson
import GHC.Generics

data Problem = Problem {
  id :: String,
  title :: String,
  contentType :: String,
  content :: String,
  createdAt :: Int,
  updatedAt :: Int,
  writer :: String,
  files :: [String],
  languages :: [String],
  tags :: [String]
} deriving (Generic)

instance ToJSON Problem
instance FromJSON Problem
