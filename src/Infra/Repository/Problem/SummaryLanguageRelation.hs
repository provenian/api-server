module Infra.Repository.Problem.SummaryLanguageRelation where

import GHC.Generics (Generic)
import Database.Generics.Mapper

data SummaryLanguageRelation = SummaryLanguageRelation {
  id :: VarChar 36 :- '["PRIMARY KEY"],
  language :: VarChar 64
} deriving (Generic, Eq, Show)
