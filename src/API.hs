module API where

import Servant
import Data.Proxy
import Driver.MySQL
import Domain.App (HandlerM, AppState(..))
import Domain.Problem (ProblemService)
import qualified Web.Handler.Problems
import qualified Web.Handler.Submissions

type API =
  "problems" :> Web.Handler.Problems.API
  :<|> "submissions" :> Web.Handler.Submissions.API

server :: ProblemService -> ServerT API HandlerM
server problemService =
  Web.Handler.Problems.api problemService :<|> Web.Handler.Submissions.api

api :: Proxy API
api = Proxy
