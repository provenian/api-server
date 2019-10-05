module Infra.Repository.ProblemRecord where

import Prelude hiding (id)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Driver.MySQL
import Database.Generics.Mapper
import Domain.App
import Domain.Model
import qualified Domain.Model.Problem.Problem as Problem

data ProblemSummaryRecord = ProblemSummaryRecord {
  id :: VarChar 36 :- '["PRIMARY KEY"],
  title :: VarChar 256,
  createdAt :: BigInt :- '["NOT NULL"],
  updatedAt :: BigInt :- '["NOT NULL"]
} deriving (Generic, Eq, Show)

{-
data ProblemRecord = ProblemRecord {
  id :: VarChar 36 :- '["PRIMARY KEY"],
  title :: VarChar 1024,
  contentType :: VarChar 128,
  content :: VarChar 1024,
  createdAt :: BigInt :- '["NOT NULL"],
  updatedAt :: BigInt :- '["NOT NULL"],
  writer :: VarChar 128,
  files :: Text,
  languages :: Text,
  tags :: Text
} deriving (Generic, Show)

toModel :: ProblemRecord -> Problem
toModel r = Problem (getVarChar $ getField $ id r)
                    (getVarChar $ title r)
                    (getVarChar $ contentType r)
                    (getVarChar $ content r)
                    (fromIntegral $ getBigInt $ getField $ createdAt r)
                    (fromIntegral $ getBigInt $ getField $ updatedAt r)
                    (getVarChar $ writer r)
                    (read $ T.unpack $ getText $ files r)
                    (read $ T.unpack $ getText $ languages r)
                    (read $ T.unpack $ getText $ tags r)

fromModel :: Problem -> ProblemRecord
fromModel r = ProblemRecord
  (Field $ VarChar $ Problem.id r)
  (VarChar $ Problem.title r)
  (VarChar $ Problem.contentType r)
  (VarChar $ Problem.content r)
  (Field $ BigInt $ fromIntegral $ Problem.createdAt r)
  (Field $ BigInt $ fromIntegral $ Problem.updatedAt r)
  (VarChar $ Problem.writer r)
  (Text $ T.pack $ show $ Problem.files r)
  (Text $ T.pack $ show $ Problem.languages r)
  (Text $ T.pack $ show $ Problem.tags r)
-}
