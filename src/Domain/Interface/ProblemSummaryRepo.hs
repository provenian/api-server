{-# LANGUAGE ConstraintKinds #-}
module Domain.Interface.ProblemSummaryRepo where

import Control.Monad.Trans.Control
import Control.Monad.Reader
import qualified Data.Text as T
import Data.Reflection (Given, given)

import Domain.App
import Domain.Model.Problem.ProblemSummary as Problem

class IProblemSummaryRepo r where
  getByID :: (MonadIO m, MonadBaseControl IO m) => r -> T.Text -> ReaderT AppState m (Maybe Problem.Summary)
  list :: (MonadIO m, MonadBaseControl IO m) => r -> ReaderT AppState m [Problem.Summary]
  save :: (MonadIO m, MonadBaseControl IO m) => r -> Problem.Summary -> ReaderT AppState m ()

data SomeProblemSummaryRepo = forall a. IProblemSummaryRepo a => SomeProblemSummaryRepo a

instance IProblemSummaryRepo SomeProblemSummaryRepo where
  getByID (SomeProblemSummaryRepo r) x = getByID r x
  list (SomeProblemSummaryRepo r) = list r
  save (SomeProblemSummaryRepo r) x = save r x

type UseProblemSummaryRepo = Given SomeProblemSummaryRepo

useProblemSummaryRepo :: UseProblemSummaryRepo => SomeProblemSummaryRepo
useProblemSummaryRepo = given
