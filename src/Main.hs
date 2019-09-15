module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Proxy
import Data.Aeson
import GHC.Generics

data Problem = Problem {
  id :: String
} deriving (Generic)

instance ToJSON Problem

type API = "problems" :> Get '[JSON] [Problem]

server :: Server API
server = return $ [Problem "12345"]

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = run 1234 app
