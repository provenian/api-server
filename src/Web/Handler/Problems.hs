module Web.Handler.Problems where

import Control.Monad.IO.Class
import Data.Aeson
import Data.UnixTime
import qualified Data.Text as T
import Foreign.C.Types (CTime(..))
import GHC.Generics
import Servant

import Domain.App
import Domain.Problem (Problem)
import qualified Domain.Problem
import Domain.InfraInterface.IProblemRepo (useProblemRepo, UseProblemRepo)
import qualified Domain.InfraInterface.IProblemRepo as IProblemRepo
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
  title :: T.Text
} deriving (Generic)

instance ToJSON CreateReq
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
    time <- fromIntegral . (\(CTime c) -> c) . utSeconds <$> liftIO getUnixTime

    IProblemRepo.create
      useProblemRepo
      (IProblemRepo.CreateInput (title req) "" "" time time "" [] [] [])
    return NoContent

  list :: HandlerM [SnakeCase Problem]
  list = fmap (map SnakeCase) $ IProblemRepo.list useProblemRepo

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
