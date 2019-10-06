module Infra.Repository.Problem.SummaryTagRelation where

import GHC.Generics (Generic)
import Database.Generics.Mapper

data SummaryTagRelation = SummaryTagRelation {
  id :: VarChar 36 :- '["NOT NULL"],
  tag :: VarChar 64
} deriving (Generic, Eq, Show)
