module Infra.Repository.ProblemRepo where

import Prelude hiding (id)
import Control.Monad.Reader
import qualified Database.MySQL.Base as SQL
import Database.Generics.Mapper as Mapper
import Data.String (IsString(fromString))
import qualified Data.Text as T
import GHC.Generics
import qualified System.IO.Streams as IOS

import Driver.MySQL
import Domain.App
import Domain.Problem (Problem(Problem))
import qualified Domain.Problem as Problem
import Domain.InfraInterface.IProblemRepo

data ProblemRecord = ProblemRecord {
  id :: VarChar 26 :- '["PRIMARY KEY"],
  title :: VarChar 1024,
  contentType :: VarChar 128,
  content :: VarChar 1024,
  createdAt :: BigInt :- '["NOT NULL"],
  updatedAt :: BigInt :- '["NOT NULL"],
  writer :: VarChar 128,
  files :: Text,
  languages :: Text,
  tags :: Text
} deriving (Generic)

createTable :: ReaderT AppState IO ()
createTable = void $ runSQL $ \conn ->
  SQL.execute_ conn $ fromString $ Mapper.createTable ProblemRecord{}

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


data Repo = Repo

new :: SomeProblemRepo
new = SomeProblemRepo Repo

instance IProblemRepo Repo where
  getByID _ key = runSQL $ \conn -> liftIO $ do
    (columns, result) <- SQL.query conn "SELECT * FROM `problem` WHERE id = ?" [SQL.MySQLText key]
    mv <- IOS.read result
    return $ fmap (toModel . deserialize) $ mv
  create _ input = runSQL $ \conn -> liftIO $ do
    (_, result) <- SQL.query conn "INSERT INTO `problem` VALUE (?,?,?,?,?,?,?,?,?,?)" (serialize $ fromModel $ fromCreateInput input "1234")
    SQL.skipToEof result
  list _ = runSQL $ \conn -> liftIO $ do
    (_, result) <- SQL.query_ conn "SELECT * FROM `problem`"
    fmap (map (toModel . deserialize)) $ IOS.toList result
