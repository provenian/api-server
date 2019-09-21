{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Domain.InfraInterface.IProblemRepo (
  CreateInput(CreateInput),
  fromCreateInput,
  IProblemRepo(..),
  SomeProblemRepo(..),
  UseProblemRepo,
  useProblemRepo,
) where

import Control.Monad.Trans.Control
import Control.Monad.Reader
import Data.Reflection (Given, given)
import Domain.App
import Domain.Problem (Problem(Problem))

data CreateInput = CreateInput {
  title :: String,
  contentType :: String,
  content :: String,
  createdAt :: Int,
  updatedAt :: Int,
  writer :: String,
  files :: [String],
  languages :: [String],
  tags :: [String]
}

fromCreateInput :: CreateInput -> String -> Problem
fromCreateInput i key = Problem key
                                (title i)
                                (contentType i)
                                (content i)
                                (createdAt i)
                                (updatedAt i)
                                (writer i)
                                (files i)
                                (languages i)
                                (tags i)

class IProblemRepo r where
  getByID :: (MonadIO m, MonadBaseControl IO m) => r -> String -> ReaderT AppState m (Maybe Problem)
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
