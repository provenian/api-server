module Domain.Model.Problem.CreateInput where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics (Generic)

data CreateInput = CreateInput {
  title :: T.Text,
  languages :: [T.Text],
  tags :: [T.Text]
} deriving (Generic)

instance FromJSON CreateInput
instance ToJSON CreateInput
