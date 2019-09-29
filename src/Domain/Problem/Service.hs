module Domain.Problem.Service where

import Control.Monad.Trans.Control
import Control.Monad.IO.Class
import Data.UnixTime
import Foreign.C.Types (CTime(..))

import Domain.App
import Domain.Problem.Model.Problem
import qualified Domain.Problem.Model.CreateInput
import qualified Domain.Problem.IProblemRepo as I

data ProblemService = I.UseProblemRepo => ProblemService

new :: I.UseProblemRepo => ProblemService
new = ProblemService

create
  :: (MonadIO m, MonadBaseControl IO m)
  => ProblemService
  -> Domain.Problem.Model.CreateInput.CreateInput
  -> AppM m ()
create ProblemService input = do
  time <- fromIntegral . (\(CTime c) -> c) . utSeconds <$> liftIO getUnixTime

  I.create
    I.useProblemRepo
    ( Domain.Problem.Model.CreateInput.CreateInput
      (Domain.Problem.Model.CreateInput.title input)
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
