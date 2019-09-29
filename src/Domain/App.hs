module Domain.App where

import Control.Monad.Reader
import Servant

import Driver.MySQL

data AppState = AppState
  { connPool :: ConnPool
  }

type AppM m = ReaderT AppState m
type HandlerM = AppM Handler

instance HasConnPool AppState where
  _ConnPool = connPool
