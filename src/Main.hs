{-# LANGUAGE FlexibleInstances #-}
module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Proxy
import Data.Aeson
import Data.Aeson.Casing

import qualified Web.Handler.Problems
import qualified Web.Handler.Submissions

type API =
  "problems" :> Web.Handler.Problems.API
  :<|> "submissions" :> Web.Handler.Submissions.API

server :: Server API
server = Web.Handler.Problems.api :<|> Web.Handler.Submissions.api

api :: Proxy API
api = Proxy

app :: Application
app = serve api server

main :: IO ()
main = run 1234 app
