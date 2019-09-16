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

rWJ :: Result
rWJ = Result
  { statusCode = "WJ"
  , statusText = "Waiting for Judge"
  , message    = ""
  , isFinished = False
  }

rCE :: String -> Result
rCE m = Result
  { statusCode = "CE"
  , statusText = "Compilation Error"
  , message    = m
  , isFinished = True
  }

rV :: String -> Result
rV m = Result
  { statusCode = "V"
  , statusText = "Verified"
  , message    = m
  , isFinished = True
  }

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
