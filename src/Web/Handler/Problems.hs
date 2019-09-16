module Web.Handler.Problems where

import Data.Aeson
import GHC.Generics
import Servant

import Domain.Problem (Problem)
import qualified Domain.Problem
import Domain.Submission (Submission)
import qualified Domain.Submission

import Web.Presenters (SnakeCase(SnakeCase))

type API =
  ReqBody '[JSON] (SnakeCase CreateReq) :> Post '[JSON] NoContent
  :<|> Get '[JSON] [SnakeCase Problem]
  :<|> "drafts" :> Get '[JSON] [SnakeCase Problem]
  :<|> Capture "problemId" String :>
    ( ReqBody '[JSON] (SnakeCase Problem) :> Put '[JSON] NoContent
    :<|> "publish" :> Put '[JSON] NoContent
    :<|> "submit" :>
      ( Get '[JSON] [SnakeCase Submission]
      :<|> ReqBody '[JSON] (SnakeCase Submission) :> Post '[JSON] NoContent
      )
    )

data CreateReq = ProblemCreateReq {
  version :: String,
  title :: String
} deriving (Generic)

instance FromJSON CreateReq

api :: Server API
api =
  post :<|> list :<|> drafts :<|> edit :<|> publish :<|> submissions :<|> submit
 where
  post :: SnakeCase CreateReq -> Handler NoContent
  post _ = return NoContent

  list :: Handler [SnakeCase Problem]
  list = return $ map
    SnakeCase
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

  drafts :: Handler [SnakeCase Problem]
  drafts = return []

  edit :: String -> SnakeCase Problem -> Handler NoContent
  edit _ _ = return NoContent

  publish :: String -> Handler NoContent
  publish _ = return NoContent

  submissions :: String -> Handler [SnakeCase Submission]
  submissions _ = return []

  submit :: String -> SnakeCase Submission -> Handler NoContent
  submit _ _ = return NoContent
