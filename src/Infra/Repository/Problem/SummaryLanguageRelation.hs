module Infra.Repository.Problem.SummaryLanguageRelation where

import GHC.Generics (Generic)
import Driver.MySQL
import Database.Generics.Mapper

data SummaryLanguageRelation = SummaryLanguageRelation {
  id :: VarChar 36 :- '["NOT NULL"],
  language :: VarChar 64
} deriving (Generic, Eq, Show)

instance TableMapper SummaryLanguageRelation
