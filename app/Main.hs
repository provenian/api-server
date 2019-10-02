module Main where

import Control.Monad.Reader
import Data.Reflection
import Network.Wai.Handler.Warp (run)
import Servant

import API (api, server)
import Domain.App
import qualified Domain.Service.Problem
import Driver.MySQL
import qualified Infra.Repository.ProblemRepo
import Infra.Repository.ProblemRecord (ProblemRecord(ProblemRecord))

problemService :: Domain.Service.Problem.ProblemService
problemService =
  give Infra.Repository.ProblemRepo.new $ Domain.Service.Problem.new

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

    runSQL $ \conn -> migrate conn ProblemRecord{}

  liftIO
    $ run 1234
    $ serve api
    $ hoistServer api (flip runReaderT appState)
    $ server problemService
