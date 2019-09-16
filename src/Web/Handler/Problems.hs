module Web.Handler.Problems where

import Data.Aeson
import GHC.Generics
import Servant

import Domain.Problem (Problem)
import qualified Domain.Problem

import Web.Presenters (SnakeCase)

type API =
  ReqBody '[JSON] (SnakeCase CreateReq) :> Post '[JSON] ()
  :<|> Get '[JSON] [Problem]

data CreateReq = ProblemCreateReq {
  version :: String,
  title :: String
} deriving (Generic)

instance FromJSON CreateReq

api :: Server API
api = post :<|> list
 where
  post :: SnakeCase CreateReq -> Handler ()
  post _ = return ()

  list :: Handler [Problem]
  list = return
    [ Domain.Problem.Problem "1234"
                             "1.0"
                             "ほげ"
                             "text/markdown"
                             "ほげぴよ"
                             1568561381
                             1568561381
                             "user"
                             ["isabelle/ROOT"]
                             ["isabelle"]
                             ["Hard"]
    ]
