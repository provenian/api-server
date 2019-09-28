{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
module Database.Generics.Mapper (
  GMapper(..),
  Mapper(..),
  (:-)(..),
  mapToSQLValues,
  mapFromSQLValues,
  createTable,
  recordTypeOf,
  RMapper,

  module Database.Generics.Mapper.MySQL,
) where

import Data.Proxy
import Data.List
import qualified Data.Map as M
import Generics.Deriving
import GHC.Generics
import GHC.TypeLits
import Database.Generics.Mapper.MySQL

data (:-) a (attrs :: [Symbol]) = Field { getField :: a }
  deriving (Eq, Show)

class GMapper f where
  grecord :: f p -> (String, [(String, String, [String])])
  gfields :: f p -> [(String, String, [String])]
  gfield :: f p -> (String, String, [String])

  gmapsTo :: f p -> [(String, SQLValue)]
  gmapsFrom :: f p -> M.Map String SQLValue -> f p

class GSelector f where
  gattrs :: f p -> (String, [String])
  gmapTo :: f p -> SQLValue
  gmapFrom :: SQLValue -> f p

instance (Datatype d, GMapper t) => GMapper (D1 d t) where
  grecord (x :: D1 d t p) = (datatypeName (undefined :: M1 _i d _f _p), gfields (unM1 x))

  gmapsTo x = gmapsTo (unM1 x)
  gmapsFrom r = M1 . gmapsFrom (unM1 r)

instance GMapper t => GMapper (C1 d t) where
  gfields x = gfields $ unM1 x

  gmapsTo x = gmapsTo (unM1 x)
  gmapsFrom r = M1 . gmapsFrom (unM1 r)

instance (GMapper r1, GMapper r2) => GMapper (r1 :*: r2) where
  gfields (r1 :*: r2) = gfields r1 ++ gfields r2

  gmapsTo (r1 :*: r2) = gmapsTo r1 ++ gmapsTo r2
  gmapsFrom (r1 :*: r2) xs = gmapsFrom r1 xs :*: gmapsFrom r2 xs

instance (Selector d, GSelector t) => GMapper (S1 d t) where
  gfields r = [gfield r]
  gfield s = let (ft,attrs) = gattrs (unM1 s) in (selName s, ft, attrs)

  gmapsTo r = [(selName r, gmapTo (unM1 r))]
  gmapsFrom r mp = maybe r (M1 . gmapFrom) (mp M.!? selName r)

instance (Mapper attrs, SQLField t) => GSelector (Rec0 (t :- attrs)) where
  gattrs (x :: Rec0 (t :- attrs) p) = (fieldType (undefined :: t), attrs (Proxy :: Proxy attrs))
  gmapTo = encode . getField . unK1
  gmapFrom = K1 . Field . decode

instance {-# OVERLAPS #-} SQLField r => GSelector (Rec0 r) where
  gattrs (x :: Rec0 r p) = (fieldType (undefined :: r), [])
  gmapTo = encode . unK1
  gmapFrom = K1 . decode

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

type RMapper a = (Generic a, GMapper (Rep a))

mapToSQLValues :: RMapper a => a -> [(String, SQLValue)]
mapToSQLValues = gmapsTo . from

mapFromSQLValues :: RMapper a => a -> M.Map String SQLValue -> a
mapFromSQLValues r = to . gmapsFrom (from r)

recordTypeOf :: RMapper a => a -> (String, M.Map String (String, [String]))
recordTypeOf =
  (\(x, y) -> (x, M.fromList $ map (\(a, b, c) -> (a, (b, c))) y))
    . grecord
    . from
