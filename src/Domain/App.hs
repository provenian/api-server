module Domain.App where

import Control.Monad.Reader
import Servant

import Driver.MySQL

data AppState = AppState
  { connPool :: ConnPool
  }

type HandlerM = ReaderT AppState Handler

instance HasConnPool AppState where
  _ConnPool = connPool
