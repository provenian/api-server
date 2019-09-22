{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Database.Generics.Mapper where

import Data.Proxy
import Data.List
import Generics.Deriving
import GHC.Generics
import GHC.TypeLits
import Database.Generics.Mapper.MySQL

data (:-) a (attrs :: [Symbol]) = Field { getField :: a }

class GMapper f where
  grecord :: f p -> (String, [(String, String, [String])])
  gfields :: f p -> [(String, String, [String])]
  gfield :: f p -> (String, String, [String])
  gattrs :: f p -> (String, [String])

instance (Datatype d, GMapper t) => GMapper (D1 d t) where
  grecord (x :: D1 d t p) = (datatypeName (undefined :: M1 _i d _f _p), gfields (unM1 x))

instance GMapper t => GMapper (C1 d t) where
  gfields x = gfields $ unM1 x

instance (GMapper r1, GMapper r2) => GMapper (r1 :*: r2) where
  gfields (r1 :*: r2) = gfields r1 ++ gfields r2

instance (Selector d, GMapper t) => GMapper (S1 d t) where
  gfields r = [gfield r]
  gfield s = let (ft,attrs) = gattrs (unM1 s) in (selName s, ft, attrs)

instance (Mapper attrs, SQLField t) => GMapper (Rec0 (t :- attrs)) where
  gattrs (x :: Rec0 (t :- attrs) p) = (fieldType (undefined :: t), attrs (Proxy :: Proxy attrs))

instance {-# OVERLAPS #-} SQLField r => GMapper (Rec0 r) where
  gattrs (x :: Rec0 r p) = (fieldType (undefined :: r), [])

class Mapper a where
  attrs :: Proxy a -> [String]

instance Mapper '[] where
  attrs Proxy = []

instance (Mapper xs, KnownSymbol x) => Mapper (x : xs) where
  attrs (Proxy :: Proxy (x:xs)) = symbolVal (Proxy :: Proxy x) : attrs (Proxy :: Proxy xs)

createTable :: (Generic a, GMapper (Rep a)) => a -> String
createTable a =
  let (tableName, fields) = grecord (from a)
  in  concat
        [ "CREATE TABLE IF NOT EXISTS `"
        , tableName
        , "` ("
        , intercalate ", " $ map
          ( \(fieldName, fieldType, attrs) -> intercalate
            " "
            ( (if null attrs then id else (++ attrs))
              ["`" ++ fieldName ++ "`", fieldType]
            )
          )
          fields
        , ")"
        ]
