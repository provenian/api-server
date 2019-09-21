module Web.Handler.Problems where

import Data.Aeson
import GHC.Generics
import Servant

import Domain.App
import Domain.Problem (Problem)
import qualified Domain.Problem
import Domain.InfraInterface.IProblemRepo
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

api :: UseProblemRepo => ServerT API HandlerM
api =
  post
    :<|> list
    :<|> drafts
    :<|> ( \problemId ->
           edit problemId
             :<|> publish problemId
             :<|> submissions problemId
             :<|> submit problemId
         )
 where
  post :: SnakeCase CreateReq -> HandlerM NoContent
  post (SnakeCase req) = do
    create useProblemRepo (CreateInput (title req) "" "" 0 0 "" [] [] [])
    return NoContent

  list :: HandlerM [SnakeCase Problem]
  list = do
    return $ map
      SnakeCase
      [ Domain.Problem.Problem "1234"
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

  drafts :: HandlerM [SnakeCase Problem]
  drafts = return []

  edit :: String -> SnakeCase Problem -> HandlerM NoContent
  edit _ _ = return NoContent

  publish :: String -> HandlerM NoContent
  publish _ = return NoContent

  submissions :: String -> HandlerM [SnakeCase Submission]
  submissions _ = return []

  submit :: String -> SnakeCase Submission -> HandlerM NoContent
  submit _ _ = return NoContent
