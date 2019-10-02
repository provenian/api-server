module Domain.Model.Problem.CreateInput where

import Data.Aeson
import Domain.Model.Problem.Problem (Problem(Problem))
import qualified Data.Text as T
import GHC.Generics (Generic)

data CreateInput = CreateInput {
  title :: T.Text,
  contentType :: T.Text,
  content :: T.Text,
  createdAt :: Int,
  updatedAt :: Int,
  writer :: T.Text,
  files :: [T.Text],
  languages :: [T.Text],
  tags :: [T.Text]
} deriving (Generic)

instance FromJSON CreateInput
instance ToJSON CreateInput

fromCreateInput :: CreateInput -> T.Text -> Problem
fromCreateInput i key = Problem key
                                (title i)
                                (contentType i)
                                (content i)
                                (createdAt i)
                                (updatedAt i)
                                (writer i)
                                (files i)
                                (languages i)
                                (tags i)

