module Infra.Repository.ProblemRepo where

import Control.Monad.Reader
import qualified Database.MySQL.Base as SQL
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
  _id :: String,
  title :: String,
  contentType :: String,
  content :: String,
  createdAt :: Int,
  updatedAt :: Int,
  writer :: String,
  files :: [String],
  languages :: [String],
  tags :: [String]
} deriving (Generic)

createTable :: ReaderT AppState IO ()
createTable = void $ runSQL $ \conn -> SQL.execute_ conn $ fromString $ concat
  [ "CREATE TABLE IF NOT EXISTS `problem` ("
  , "id VARCHAR(26) PRIMARY KEY, "
  , "title VARCHAR(1024), "
  , "contentType VARCHAR(128), "
  , "content VARCHAR(1024), "
  , "createdAt bigint NOT NULL, "
  , "updatedAt bigint NOT NULL, "
  , "writer VARCHAR(128), "
  , "files text DEFAULT NULL, "
  , "languages text DEFAULT NULL, "
  , "tags text DEFAULT NULL"
  , ")"
  ]

mapToRecord :: [SQL.MySQLValue] -> ProblemRecord
mapToRecord [vId, vTitle, vContentType, vContent, vCreatedAt, vUpdatedAt, vWriter, vFiles, vLangs, vTags]
  = ProblemRecord (unwrapString vId)
                  (unwrapString vTitle)
                  (unwrapString vContentType)
                  (unwrapString vContent)
                  (unwrapInt vCreatedAt)
                  (unwrapInt vUpdatedAt)
                  ""
                  []
                  []
                  []
 where
  unwrapString (SQL.MySQLText t) = T.unpack t
  unwrapInt (SQL.MySQLInt64 i) = fromIntegral $ toInteger i

mapToRawValues :: ProblemRecord -> [SQL.MySQLValue]
mapToRawValues r =
  [ wrapString (_id r)
  , wrapString (title r)
  , wrapString (contentType r)
  , wrapString (content r)
  , wrapInt (createdAt r)
  , wrapInt (updatedAt r)
  , wrapString (writer r)
  , wrapString $ concat (files r)
  , wrapString $ concat (languages r)
  , wrapString $ concat (tags r)
  ]
 where
  wrapString = SQL.MySQLText . T.pack

  wrapInt :: Int -> SQL.MySQLValue
  wrapInt = SQL.MySQLInt64 . fromIntegral

toModel :: ProblemRecord -> Problem
toModel r = Problem (_id r)
                    (title r)
                    (contentType r)
                    (content r)
                    (createdAt r)
                    (updatedAt r)
                    (writer r)
                    (files r)
                    (languages r)
                    (tags r)

fromModel :: Problem -> ProblemRecord
fromModel r = ProblemRecord (Problem.id r)
                            (Problem.title r)
                            (Problem.contentType r)
                            (Problem.content r)
                            (Problem.createdAt r)
                            (Problem.updatedAt r)
                            (Problem.writer r)
                            (Problem.files r)
                            (Problem.languages r)
                            (Problem.tags r)


data Repo = Repo

new :: SomeProblemRepo
new = SomeProblemRepo Repo

instance IProblemRepo Repo where
  getByID _ key = runSQL $ \conn -> liftIO $ do
    (columns, result) <- SQL.query conn "SELECT * FROM `problem` WHERE id = ?" [SQL.MySQLText $ T.pack key]
    mv <- IOS.read result
    return $ fmap (toModel . mapToRecord) $ mv
  create _ input = runSQL $ \conn -> liftIO $ do
    (_, result) <- SQL.query conn "INSERT INTO `problem` VALUE (?,?,?,?,?,?,?,?,?,?)" (mapToRawValues $ fromModel $ fromCreateInput input "1234")
    SQL.skipToEof result
  list _ = runSQL $ \conn -> liftIO $ do
    (_, result) <- SQL.query_ conn "SELECT * FROM `problem`"
    fmap (map (toModel . mapToRecord)) $ IOS.toList result
