{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.Generics.Mapper.MySQL where

import Data.Int
import Data.Proxy
import qualified Data.Text as T
import GHC.TypeLits

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

newtype VarChar (length :: Nat) = VarChar { getText :: T.Text }

instance KnownNat n => SQLField (VarChar n) where
  fieldType (_ :: VarChar n) = "varchar(" ++ show (natVal (Proxy :: Proxy n)) ++ ")"

newtype BigInt = BigInt { getBigInt :: Int64 }

instance SQLField BigInt where
  fieldType _ = "bigint"
