module Domain.Service.Problem where

import Control.Monad.Trans.Control
import Control.Monad.IO.Class
import Data.UnixTime
import Foreign.C.Types (CTime(..))

import Domain.App
import Domain.Model.Problem.Problem
import qualified Domain.Model.Problem.CreateInput
import qualified Domain.Interface.IProblemRepo as I

data ProblemService = I.UseProblemRepo => ProblemService

new :: I.UseProblemRepo => ProblemService
new = ProblemService

create
  :: (MonadIO m, MonadBaseControl IO m)
  => ProblemService
  -> Domain.Model.Problem.CreateInput.CreateInput
  -> AppM m ()
create ProblemService input = do
  time <- fromIntegral . (\(CTime c) -> c) . utSeconds <$> liftIO getUnixTime

  I.create
    I.useProblemRepo
    ( Domain.Model.Problem.CreateInput.CreateInput
      (Domain.Model.Problem.CreateInput.title input)
      ""
      ""
      time
      time
      ""
      []
      []
      []
    )

list :: (MonadIO m, MonadBaseControl IO m) => ProblemService -> AppM m [Problem]
list ProblemService = I.list I.useProblemRepo
