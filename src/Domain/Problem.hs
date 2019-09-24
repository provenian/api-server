module Domain.Problem where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

data Problem = Problem {
  id :: T.Text,
  title :: T.Text,
  contentType :: T.Text,
  content :: T.Text,
  createdAt :: Int,
  updatedAt :: Int,
  writer :: T.Text,
  files :: [T.Text],
  languages :: [T.Text],
  tags :: [T.Text]
} deriving (Generic, Show)

instance ToJSON Problem
instance FromJSON Problem
