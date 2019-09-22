module Database.Generics.Mapper.MySQL where

import Data.Int

class SQLField a where
  fieldType :: a -> String

instance SQLField String where
  fieldType _ = "text"

instance SQLField Int where
  fieldType _ = "bigint"

instance SQLField Int64 where
  fieldType _ = "bigint"

instance SQLField Int32 where
  fieldType _ = "bigint"
