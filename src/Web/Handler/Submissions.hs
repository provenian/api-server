module Web.Handler.Submissions where

import Servant

import Domain.App
import Domain.Submission (Submission)
import qualified Domain.Submission
import Web.Presenters (SnakeCase(SnakeCase))

type API =
  Capture "submissionId" String :> Get '[JSON] (SnakeCase Submission)

api :: ServerT API HandlerM
api = get
 where
  get :: String -> HandlerM (SnakeCase Submission)
  get _ = return $ SnakeCase $ Domain.Submission.Submission
    "1234"
    12345
    "12345"
    "fooo"
    "isabelle"
    "user1"
    Domain.Submission.rWJ
