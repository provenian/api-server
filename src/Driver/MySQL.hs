{-# LANGUAGE FlexibleContexts #-}
module Driver.MySQL (
  runSQL,
  createSQLPool,
  ConnPool,
  HasConnPool(..),

  SQL.ConnectInfo(..),
  SQL.defaultConnectInfo,
  MySQL.Option(..),
  GWrapper(..),
  queryWith,
  queryWith_,
  createTableOf,
  createTable,
  createTableWithName,
  migrateColumn,
  migrate,
  insertInto,
) where

import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Data.Pool
import Data.String (IsString(fromString))
import qualified Data.Map as M
import qualified Data.Binary.Builder as Builder
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.Encoding as TE
import qualified Database.MySQL.Simple as SQL
import qualified Database.MySQL.Simple.Result as SQL
import qualified Database.MySQL.Simple.QueryParams as SQL
import qualified Database.MySQL.Simple.QueryResults as SQL
import qualified Database.MySQL.Simple.Param as SQL
import qualified Database.MySQL.Base.Types as MySQL
import Database.Generics.Mapper
import GHC.Generics

newtype ConnPool = ConnPool { getPool :: Pool SQL.Connection }

class HasConnPool c where
  _ConnPool :: c -> ConnPool

runSQL
  :: (Monad m, MonadBaseControl IO m, HasConnPool c)
  => (SQL.Connection -> m a)
  -> ReaderT c m a
runSQL m = ask >>= \s -> withResource (getPool $ _ConnPool s) (lift . m)

createSQLPool :: MonadIO m => SQL.ConnectInfo -> Int -> m ConnPool
createSQLPool conn m =
  liftIO $ fmap ConnPool $ createPool (SQL.connect conn) SQL.close 1 5 m

instance SQL.Param SQLValue where
  render (SQLVarChar c) = SQL.render c
  render (SQLText c) = SQL.render c
  render (SQLBigInt c) = SQL.render c
  render (SQLInt c) = SQL.render c

newtype GWrapper = GWrapper { getGWrapper :: [(T.Text, Maybe SQLValue)] }

instance SQL.QueryResults GWrapper where
  convertResults x y = GWrapper $ zipWith (\x y -> (TE.decodeUtf8 $ MySQL.fieldName x, fmap (go (MySQL.fieldType x)) y)) x y where
    go MySQL.VarString bs = SQLVarChar $ TE.decodeUtf8 bs
    go MySQL.Blob bs = SQLText $ TE.decodeUtf8 bs
    go MySQL.LongLong bs = SQLBigInt $ (\(Right (x,"")) -> x) $ T.decimal $ TE.decodeUtf8 bs
    go MySQL.Long bs = SQLInt $ (\(Right (x,"")) -> x) $ T.decimal $ TE.decodeUtf8 bs
    go x y = error ("unsupported type: " ++ show x ++ "; " ++ show y)

queryWith
  :: (SQL.QueryParams q, RMapper r)
  => SQL.Connection
  -> SQL.Query
  -> q
  -> r  -- ^ record of "default value"
  -> IO [r]
queryWith conn q p r = do
  ws <- SQL.query conn q p
  return $ map
    ( mapFromSQLValues r
    . M.fromList
    . concatMap (\(x, y) -> maybe [] (return . (,) x) y)
    . getGWrapper
    )
    ws

queryWith_
  :: (RMapper r)
  => SQL.Connection
  -> SQL.Query
  -> r  -- ^ record of "default value"
  -> IO [r]
queryWith_ conn q r = do
  ws <- SQL.query_ conn q
  return $ map
    ( mapFromSQLValues r
    . M.fromList
    . concatMap (\(x, y) -> maybe [] (return . (,) x) y)
    . getGWrapper
    )
    ws

createTableOf :: T.Text -> [(T.Text, T.Text, [T.Text])] -> T.Text
createTableOf tableName fields = T.concat
  [ "CREATE TABLE IF NOT EXISTS `"
  , tableName
  , "` ("
  , T.intercalate ", " $ map
    ( \(fieldName, fieldType, attrs) -> T.intercalate
      " "
      ( (if null attrs then id else (++ attrs))
        ["`" `T.append` fieldName `T.append` "`", fieldType]
      )
    )
    fields
  , ")"
  ]

createTable :: (Generic a, GMapper (Rep a)) => a -> T.Text
createTable a = uncurry createTableOf (grecord (from a))

createTableWithName :: (Generic a, GMapper (Rep a)) => T.Text -> a -> T.Text
createTableWithName tableName a =
  uncurry (\_ -> createTableOf tableName) (grecord (from a))

migrateColumn
  :: SQL.Connection
  -> T.Text  -- ^ table name
  -> T.Text  -- ^ column name
  -> T.Text  -- ^ field type
  -> [T.Text]  -- ^ field attributes
  -> IO ()
migrateColumn conn tableName columnName field attrs = do
  [SQL.Only c] <- SQL.query
    conn
    "select COLUMN_TYPE from INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = ? and COLUMN_NAME = ?"
    (tableName, columnName)

  -- not checking field attrs here!
  when (c /= field) $ do
    void $ SQL.execute_ conn $ fromString $ T.unpack $ T.concat
      [ "alter table `"
      , tableName
      , "` modify column "
      , columnName
      , " "
      , field
      , " "
      , T.intercalate " " attrs
      ]

-- ALTER TABLE table_name DROP PRIMARY KEY, ADD PRIMARY KEY (id);

migrate :: (Generic a, GMapper (Rep a)) => SQL.Connection -> a -> IO ()
migrate conn entity = do
  let (tableName, fields) = grecord $ from entity

  forM_ fields $ \(fieldName, fieldType, attrs) ->
    migrateColumn conn tableName fieldName fieldType attrs

insertInto :: (RMapper a) => SQL.Connection -> T.Text -> a -> IO Int
insertInto conn tableName val = do
  let pairs   = mapToSQLValues val
  let columns = map fst pairs
  let values  = map snd pairs

  fmap fromIntegral $ SQL.execute
    conn
    (          fromString
    $          T.unpack
    $          "INSERT INTO `"
    `T.append` tableName
    `T.append` "` ("
    `T.append` T.intercalate "," columns
    `T.append` ") VALUES (?)"
    )
    (SQL.Only $ SQL.VaArgs values)
