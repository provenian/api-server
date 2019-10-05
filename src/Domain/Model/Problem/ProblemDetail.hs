module Domain.Model.Problem.ProblemDetail where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

data ProblemDetail = ProblemDetail {
  id :: T.Text,
  title :: T.Text,
  contentType :: T.Text,
  content :: T.Text,
  createdAt :: Int,
  updatedAt :: Int,
  writer :: T.Text,
  languages :: [T.Text],
  tags :: [T.Text]
} deriving (Generic, Eq, Show)

instance ToJSON ProblemDetail
instance FromJSON ProblemDetail
