module Domain.Problem.Service where

import Control.Monad.IO.Class
import Data.UnixTime
import Foreign.C.Types (CTime(..))

import Domain.App
import Domain.Problem.Model.Problem
import qualified Domain.Problem.Model.CreateInput
import qualified Domain.Problem.IProblemRepo as I

data ProblemService = I.UseProblemRepo => ProblemService

create
  :: ProblemService
  -> Domain.Problem.Model.CreateInput.CreateInput
  -> HandlerM ()
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

list :: ProblemService -> HandlerM [Problem]
list ProblemService = I.list I.useProblemRepo
