{-# LANGUAGE FlexibleContexts #-}
module Driver.MySQL (
  runSQL,
  createSQLPool,
  ConnPool,
  HasConnPool(..),

  SQL.ConnectInfo(..),
  SQL.defaultConnectInfoMB4,

  serialize,
  deserialize,
) where

import Control.Monad.Reader
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Data.Pool
import qualified Database.MySQL.Base as SQL
import Database.Generics.Mapper
import GHC.Generics

newtype ConnPool = ConnPool { getPool :: Pool SQL.MySQLConn }

class HasConnPool c where
  _ConnPool :: c -> ConnPool

runSQL
  :: (Monad m, MonadBaseControl IO m, HasConnPool c)
  => (SQL.MySQLConn -> m a)
  -> ReaderT c m a
runSQL m = ask >>= \s -> withResource (getPool $ _ConnPool s) (lift . m)

createSQLPool :: MonadIO m => SQL.ConnectInfo -> Int -> m ConnPool
createSQLPool conn m =
  liftIO $ fmap ConnPool $ createPool (SQL.connect conn) SQL.close 1 5 m

serialize :: (Generic a, GMapper (Rep a)) => a -> [SQL.MySQLValue]
serialize = map asMySQL . mapToSQLValues

deserialize :: (Generic a, GMapper (Rep a)) => [SQL.MySQLValue] -> a
deserialize = mapFromSQLValues . map fromMySQL

asMySQL :: SQLValue -> SQL.MySQLValue
asMySQL (SQLText t) = SQL.MySQLText t

fromMySQL :: SQL.MySQLValue -> SQLValue
fromMySQL (SQL.MySQLText t) = SQLText t
