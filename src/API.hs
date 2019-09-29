module API where

import Servant
import Data.Proxy
import Driver.MySQL
import Domain.App (HandlerM, AppState(..))
import Domain.InfraInterface.IProblemRepo
import qualified Web.Handler.Problems
import qualified Web.Handler.Submissions

type API =
  "problems" :> Web.Handler.Problems.API
  :<|> "submissions" :> Web.Handler.Submissions.API

server :: UseProblemRepo => ServerT API HandlerM
server = Web.Handler.Problems.api :<|> Web.Handler.Submissions.api

api :: Proxy API
api = Proxy
