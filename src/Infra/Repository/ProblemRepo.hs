module Infra.Repository.ProblemRepo where

import Control.Monad.Reader
import Data.List (intercalate)
import Data.String (IsString(fromString))

import Database.Generics.Mapper as Mapper
import qualified Database.MySQL.Simple as SQL
import Driver.MySQL
import Domain.App
import Domain.Interface.IProblemRepo
import qualified Domain.Model.Problem.CreateInput as CreateInput
import Infra.Repository.ProblemRecord

data Repo = Repo

new :: SomeProblemRepo
new = SomeProblemRepo Repo

createTable :: ReaderT AppState IO ()
createTable = void $ runSQL $ \conn ->
  SQL.execute_ conn $ fromString $ Mapper.createTable ProblemRecord{}

instance IProblemRepo Repo where
  getByID _ key = runSQL $ \conn -> liftIO $ do
    result <- queryWith conn "SELECT * FROM `ProblemRecord` WHERE id = ?" [key] ProblemRecord{}
    return $ (\xs -> if length xs == 1 then Just (head xs) else Nothing) $ map toModel $ result
  create _ input = runSQL $ \conn -> liftIO $ do
    let pairs = mapToSQLValues $ fromModel $ CreateInput.fromCreateInput input "1234"
    let columns = map fst pairs
    let values = map snd pairs
    _ <- SQL.execute conn (fromString $ "INSERT INTO `ProblemRecord` (" ++ intercalate "," columns ++ ") VALUES (?)") (SQL.Only $ SQL.VaArgs values)
    return ()
  list _ = fmap (map toModel) $ runSQL $ \conn -> liftIO $ queryWith_ conn "SELECT * FROM `ProblemRecord`" ProblemRecord{}
