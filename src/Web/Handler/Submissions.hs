module Web.Handler.Submissions where

import Servant

import Domain.Submission (Submission)
import qualified Domain.Submission
import Web.Presenters (SnakeCase(SnakeCase))

type API =
  Capture "submissionId" String :> Get '[JSON] (SnakeCase Submission)

api :: Server API
api = get
 where
  get :: String -> Handler (SnakeCase Submission)
  get _ = return $ SnakeCase $ Domain.Submission.Submission
    "1234"
    12345
    "12345"
    "fooo"
    "isabelle"
    "user1"
    Domain.Submission.rWJ
