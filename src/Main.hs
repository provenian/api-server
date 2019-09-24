module Main where

import Control.Monad.Reader
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Proxy
import Data.Reflection
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Servant

import Driver.MySQL
import Domain.App (HandlerM, AppState(..))
import Domain.InfraInterface.IProblemRepo
import qualified Web.Handler.Problems
import qualified Web.Handler.Submissions
import qualified Infra.Repository.ProblemRepo

type API =
  "problems" :> Web.Handler.Problems.API
  :<|> "submissions" :> Web.Handler.Submissions.API

server :: UseProblemRepo => ServerT API HandlerM
server = Web.Handler.Problems.api :<|> Web.Handler.Submissions.api

api :: Proxy API
api = Proxy

main :: IO ()
main = give Infra.Repository.ProblemRepo.new $ do
  pool <- createSQLPool
    ( defaultConnectInfo { connectDatabase = "provenian"
                         , connectUser     = "root"
                         , connectPassword = "password"
                         , connectHost     = "127.0.0.1"
                         , connectOptions  = [CharsetName "utf8mb4"]
                         }
    )
    10
  let appState = AppState {connPool = pool}

  flip runReaderT appState $ do
    Infra.Repository.ProblemRepo.createTable

  liftIO $ run 1234 $ serve api $ hoistServer api
                                              (flip runReaderT appState)
                                              server
