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
) where

import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Data.Pool
import Data.List
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

instance {-# OVERLAPS #-} SQL.Result (Maybe SQLValue) where
  convert f bs = fmap (go (MySQL.fieldType f)) bs where
    go MySQL.VarString bs = SQLVarChar $ TE.decodeUtf8 bs
    go MySQL.Blob bs = SQLText $ TE.decodeUtf8 bs
    go MySQL.LongLong bs = SQLBigInt $ (\(Right (x,"")) -> x) $ T.decimal $ TE.decodeUtf8 bs
    go MySQL.Long bs = SQLInt $ (\(Right (x,"")) -> x) $ T.decimal $ TE.decodeUtf8 bs
    go x y = error ("unsupported type: " ++ show f ++ "; " ++ show bs)

instance SQL.Result a => SQL.QueryResults [a] where
  convertResults = zipWith SQL.convert

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
    . map (\(x, y) -> (T.unpack x, y))
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
    . map (\(x, y) -> (T.unpack x, y))
    . getGWrapper
    )
    ws
