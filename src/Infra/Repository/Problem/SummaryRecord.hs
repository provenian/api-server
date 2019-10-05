module Infra.Repository.Problem.SummaryRecord where

import GHC.Generics (Generic)
import Database.Generics.Mapper

data SummaryRecord = SummaryRecord {
  id :: VarChar 36 :- '["PRIMARY KEY"],
  title :: VarChar 256,
  createdAt :: BigInt :- '["NOT NULL"],
  updatedAt :: BigInt :- '["NOT NULL"]
} deriving (Generic, Eq, Show)
