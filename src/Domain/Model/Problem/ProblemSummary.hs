module Domain.Model.Problem.ProblemSummary where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

data Summary = Summary {
  id :: T.Text,
  title :: T.Text,
  createdAt :: Int,
  updatedAt :: Int,
  languages :: [T.Text],
  tags :: [T.Text]
} deriving (Generic, Eq, Show)

instance ToJSON Summary
instance FromJSON Summary
