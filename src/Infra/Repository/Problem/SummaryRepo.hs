module Infra.Repository.Problem.SummaryRepo where

import Control.Monad.Reader
import Data.Maybe.Only
import qualified Data.Text as T
import Data.String (IsString(fromString))
import Database.Generics.Mapper
import qualified Database.MySQL.Simple as SQL
import Driver.MySQL
import Domain.App
import Domain.Interface.ProblemSummaryRepo
import Domain.Model.Problem.ProblemSummary (Summary(Summary))
import qualified Domain.Model.Problem.ProblemSummary as Problem
import Infra.Repository.Problem.SummaryRecord (SummaryRecord(SummaryRecord))
import Infra.Repository.Problem.SummaryLanguageRelation (SummaryLanguageRelation(SummaryLanguageRelation))
import Infra.Repository.Problem.SummaryTagRelation (SummaryTagRelation(SummaryTagRelation))
import qualified Infra.Repository.Problem.SummaryRecord as SummaryRecord
import qualified Infra.Repository.Problem.SummaryLanguageRelation as SummaryLanguageRelation
import qualified Infra.Repository.Problem.SummaryTagRelation as SummaryTagRelation

createTable :: ReaderT AppState IO ()
createTable = void $ runSQL $ \conn -> do
  SQL.execute_ conn $ fromString $ T.unpack $ Driver.MySQL.createTableWithName
    "problem_summary"
    SummaryRecord{}
  SQL.execute_ conn $ fromString $ T.unpack $ Driver.MySQL.createTableWithName
    "problem_summary_language_relation"
    SummaryLanguageRelation{}
  SQL.execute_ conn $ fromString $ T.unpack $ Driver.MySQL.createTableWithName
    "problem_summary_tag_relation"
    SummaryTagRelation{}

migrateTable :: ReaderT AppState IO ()
migrateTable = void $ runSQL $ \conn -> do
  Driver.MySQL.migrateWithName conn "problem_summary" SummaryRecord{}
  Driver.MySQL.migrateWithName conn
                               "problem_summary_language_relation"
                               SummaryLanguageRelation{}
  Driver.MySQL.migrateWithName conn
                               "problem_summary_tag_relation"
                               SummaryTagRelation{}

fromModel
  :: Summary -> (SummaryRecord, [SummaryLanguageRelation], [SummaryTagRelation])
fromModel m =
  let rid = VarChar $ Problem.id m
  in  ( SummaryRecord (Field rid)
                      (VarChar $ Problem.title m)
                      (Field $ BigInt $ fromIntegral $ Problem.createdAt m)
                      (Field $ BigInt $ fromIntegral $ Problem.updatedAt m)
      , map (\t -> SummaryLanguageRelation (Field rid) (VarChar t))
        $ Problem.languages m
      , map (\t -> SummaryTagRelation (Field rid) (VarChar t)) $ Problem.tags m
      )

-- | Won't check the equality of id
toModel
  :: SummaryRecord
  -> [SummaryLanguageRelation]
  -> [SummaryTagRelation]
  -> Summary
toModel r langs tags = Summary
  (getVarChar $ getField $ SummaryRecord.id r)
  (getVarChar $ SummaryRecord.title r)
  (fromIntegral $ getBigInt $ getField $ SummaryRecord.createdAt r)
  (fromIntegral $ getBigInt $ getField $ SummaryRecord.updatedAt r)
  (map (\u -> getVarChar $ SummaryLanguageRelation.language u) langs)
  (map (\u -> getVarChar $ SummaryTagRelation.tag u) tags)

data Repo = Repo

new :: SomeProblemSummaryRepo
new = SomeProblemSummaryRepo Repo

instance IProblemSummaryRepo Repo where
  -- use join
  getByID _ key = runSQL $ \conn -> liftIO $ do
    mSummary <- fmap mayOnly $ queryWith conn "SELECT * FROM `problem_summary` WHERE id = ?" [key] SummaryRecord{}
    langs <- queryWith conn "SELECT * FROM `problem_summary_language_relation` WHERE id = ?" [key] SummaryLanguageRelation{}
    tags <- queryWith conn "SELECT * FROM `problem_summary_tag_relation` WHERE id = ?" [key] SummaryTagRelation{}
    return $ fmap (\r -> toModel r langs tags) mSummary

  list _ = runSQL $ \conn -> liftIO $ do
    summaries <- queryWith_ conn "SELECT * FROM `problem_summary`" SummaryRecord{}
    return $ map (\s -> toModel s [] []) summaries

  save _ m = runSQL $ \conn -> liftIO $ do
    let (r, langs, tags) = fromModel m

    void $ insertInto conn "problem_summary" r
    forM_ langs $ void . insertInto conn "problem_summary_language_relation"
    forM_ tags $ void . insertInto conn "problem_summary_tag_relation"
