module Infra.Repository.ProblemRepo where

import Control.Monad.Reader
import Data.List (intercalate)
import Data.String (IsString(fromString))
import qualified Data.Text as T
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

import Database.Generics.Mapper
import qualified Database.MySQL.Simple as SQL
import Driver.MySQL
import Domain.App
import Domain.Interface.IProblemRepo
import qualified Domain.Model.Problem.CreateInput as CreateInput
import Infra.Repository.ProblemRecord

{-
data Repo = Repo

new :: SomeProblemRepo
new = SomeProblemRepo Repo

createTable :: ReaderT AppState IO ()
createTable = void $ runSQL $ \conn ->
  SQL.execute_ conn $ fromString $ T.unpack $ Driver.MySQL.createTable
    ProblemRecord{}

instance IProblemRepo Repo where
  getByID _ key = runSQL $ \conn -> liftIO $ do
    result <- queryWith conn "SELECT * FROM `ProblemRecord` WHERE id = ?" [key] ProblemRecord{}
    return $ (\xs -> if length xs == 1 then Just (head xs) else Nothing) $ map toModel $ result

  create _ input = runSQL $ \conn -> liftIO $ do
    uuid <- UUID.toText <$> UUID.nextRandom
    let pairs = mapToSQLValues $ fromModel $ CreateInput.fromCreateInput input uuid
    let columns = map fst pairs
    let values = map snd pairs
    _ <- SQL.execute conn (fromString $ T.unpack $ "INSERT INTO `ProblemRecord` (" `T.append` T.intercalate "," columns `T.append` ") VALUES (?)") (SQL.Only $ SQL.VaArgs values)
    return ()

  list _ = fmap (map toModel) $ runSQL $ \conn -> liftIO $ queryWith_ conn "SELECT * FROM `ProblemRecord`" ProblemRecord{}
-}
