{-# LANGUAGE FlexibleContexts #-}
module Driver.MySQL (
  runSQL,
  createSQLPool,
  ConnPool,
  HasConnPool(..),

  SQL.ConnectInfo(..),
  SQL.defaultConnectInfo,
  MySQL.Option(..),
) where

import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Data.Pool
import qualified Data.Binary.Builder as Builder
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.Encoding as TE
import qualified Database.MySQL.Simple as SQL
import qualified Database.MySQL.Simple.Result as SQL
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
