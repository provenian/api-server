module Domain.Submission where

import Data.Aeson hiding (Result)
import GHC.Generics

data Result = Result {
  statusCode :: String,
  statusText :: String,
  message :: String,
  isFinished :: Bool
} deriving (Generic)

instance ToJSON Result
instance FromJSON Result

data Submission = Submission {
  id :: String,
  createdAt :: Int,
  problemId :: String,
  code :: String,
  language :: String,
  userId :: String,
  result :: Result
} deriving (Generic)

instance ToJSON Submission
instance FromJSON Submission
