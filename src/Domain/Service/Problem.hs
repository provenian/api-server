module Domain.Service.Problem where

import Control.Monad.Trans.Control
import Control.Monad.IO.Class
import Data.UnixTime
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID
import Foreign.C.Types (CTime(..))

import Domain.App
import Domain.Model.Problem.Problem
import Domain.Model.Problem.ProblemSummary (Summary(Summary))
import qualified Domain.Model.Problem.CreateInput as CreateInput
import qualified Domain.Interface.ProblemSummaryRepo as I

data ProblemService = I.UseProblemSummaryRepo => ProblemService

new :: I.UseProblemSummaryRepo => ProblemService
new = ProblemService

create
  :: (MonadIO m, MonadBaseControl IO m)
  => ProblemService
  -> CreateInput.CreateInput
  -> AppM m ()
create ProblemService input = do
  uuid <- liftIO $ UUID.toText <$> UUID.nextRandom
  time <- fromIntegral . (\(CTime c) -> c) . utSeconds <$> liftIO getUnixTime

  I.save I.useProblemSummaryRepo $ Summary uuid
                                           (CreateInput.title input)
                                           time
                                           time
                                           (CreateInput.languages input)
                                           (CreateInput.tags input)

listSummary
  :: (MonadIO m, MonadBaseControl IO m) => ProblemService -> AppM m [Summary]
listSummary ProblemService = I.list I.useProblemSummaryRepo
