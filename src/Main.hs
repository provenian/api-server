{-# LANGUAGE FlexibleInstances #-}
module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Proxy
import Data.Aeson
import Data.Aeson.Casing

import qualified Web.Handler.Problems

type API =
  "problems" :> Web.Handler.Problems.API
  :<|> "submissions" :>
    (
      Capture "submissionId" String :> Get '[JSON] ()
    )

server :: Server API
server = Web.Handler.Problems.api :<|> (\_ -> return ())

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = run 1234 app
