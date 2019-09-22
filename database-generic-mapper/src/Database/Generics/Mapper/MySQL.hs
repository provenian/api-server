{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Database.Generics.Mapper.MySQL where

import Data.Int
import Data.Word
import Data.Proxy
import qualified Data.Text as T
import GHC.TypeLits hiding (Text)

class SQLField a where
  fieldType :: a -> String
  encode :: a -> SQLValue
  decode :: SQLValue -> a

instance SQLField String where
  fieldType _ = "text"
  encode = SQLText . T.pack
  decode = T.unpack . (\(SQLText t) -> t)

instance SQLField Int where
  fieldType _ = "bigint"
  encode = SQLBigInt . fromIntegral
  decode = fromIntegral . (\(SQLBigInt t) -> t)

instance SQLField Int64 where
  fieldType _ = "bigint"
  encode = SQLBigInt . fromIntegral
  decode = fromIntegral . (\(SQLBigInt t) -> t)

instance SQLField Int32 where
  fieldType _ = "int"
  encode = SQLInt
  decode = (\(SQLInt t) -> t)

newtype VarChar (length :: Nat) = VarChar { getVarChar :: T.Text }
  deriving (Eq, Show)

instance KnownNat n => SQLField (VarChar n) where
  fieldType (_ :: VarChar n) = "varchar(" ++ show (natVal (Proxy :: Proxy n)) ++ ")"
  encode = SQLVarChar . getVarChar
  decode = VarChar . (\(SQLVarChar c) -> c)

newtype BigInt = BigInt { getBigInt :: Int64 }
  deriving (Eq, Show)

instance SQLField BigInt where
  fieldType _ = "bigint"
  encode = SQLBigInt . getBigInt
  decode = BigInt . (\(SQLBigInt t) -> t)

newtype Text = Text { getText :: T.Text }
  deriving (Eq, Show)

instance SQLField Text where
  fieldType _ = "text"
  encode = SQLText . getText
  decode = Text . (\(SQLText t) -> t)

data SQLValue
  = SQLBool Bool
  | SQLTinyInt Int8
  | SQLTinyUInt Word8
  | SQLSmallInt Int16
  | SQLSmallUInt Word16
  | SQLInt Int32
  | SQLUInt Word32
  | SQLBigInt Int64
  | SQLBigUInt Word64
  | SQLFloat Float
  | SQLDouble Double
  | SQLVarChar T.Text
  | SQLText T.Text
  deriving (Eq, Show)
