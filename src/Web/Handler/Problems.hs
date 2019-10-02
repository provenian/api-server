module Web.Handler.Problems where

import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import Servant

import Domain.App
import Domain.Model
import Domain.Interface.IProblemRepo (useProblemRepo, UseProblemRepo)
import Domain.Service.Problem (ProblemService)
import qualified Domain.Service.Problem as ProblemService
import qualified Domain.Interface.IProblemRepo as IProblemRepo
import Domain.Submission (Submission)
import qualified Domain.Submission

import Web.Presenters (SnakeCase(SnakeCase))

type API =
  ReqBody '[JSON] (SnakeCase CreateInput) :> Post '[JSON] NoContent
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

api :: ProblemService -> ServerT API HandlerM
api problemService =
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
  post :: SnakeCase CreateInput -> HandlerM NoContent
  post (SnakeCase req) = do
    ProblemService.create problemService req
    return NoContent

  list :: HandlerM [SnakeCase Problem]
  list = fmap (map SnakeCase) $ ProblemService.list problemService

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
