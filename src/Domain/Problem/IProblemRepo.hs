{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Domain.Problem.IProblemRepo (
  IProblemRepo(..),
  SomeProblemRepo(..),
  UseProblemRepo,
  useProblemRepo,
) where

import Control.Monad.Trans.Control
import Control.Monad.Reader
import qualified Data.Text as T
import Data.Reflection (Given, given)
import Domain.App
import Domain.Problem.Model.Problem (Problem)
import Domain.Problem.Model.CreateInput (CreateInput)

class IProblemRepo r where
  getByID :: (MonadIO m, MonadBaseControl IO m) => r -> T.Text -> ReaderT AppState m (Maybe Problem)
  create :: (MonadIO m, MonadBaseControl IO m) => r -> CreateInput -> ReaderT AppState m ()
  list :: (MonadIO m, MonadBaseControl IO m) => r -> ReaderT AppState m [Problem]

data SomeProblemRepo = forall a. IProblemRepo a => SomeProblemRepo a

instance IProblemRepo SomeProblemRepo where
  getByID (SomeProblemRepo r) x = getByID r x
  create (SomeProblemRepo r) x = create r x
  list (SomeProblemRepo r) = list r

type UseProblemRepo = Given SomeProblemRepo

useProblemRepo :: UseProblemRepo => SomeProblemRepo
useProblemRepo = given
