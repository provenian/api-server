module Main where

import Control.Monad.Reader
import Data.Reflection
import Network.Wai.Handler.Warp (run)
import Servant

import API (api, server)
import Domain.App
import qualified Domain.Problem.Service
import Driver.MySQL
import Infra.Repository.ProblemRepo

problemService :: Domain.Problem.Service.ProblemService
problemService =
  give Infra.Repository.ProblemRepo.new $ Domain.Problem.Service.new

main :: IO ()
main = do
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

  liftIO
    $ run 1234
    $ serve api
    $ hoistServer api (flip runReaderT appState)
    $ server problemService
