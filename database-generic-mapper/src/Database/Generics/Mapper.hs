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
  recordTypeOf,
  RMapper,

  module Database.Generics.Mapper.MySQL,
) where

import Data.Proxy
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Generics.Deriving
import GHC.Generics
import GHC.TypeLits
import Database.Generics.Mapper.MySQL

data (:-) a (attrs :: [Symbol]) = Field { getField :: a }
  deriving (Eq, Show)

class GMapper f where
  grecord :: f p -> (T.Text, [(T.Text, T.Text, [T.Text])])
  gfields :: f p -> [(T.Text, T.Text, [T.Text])]
  gfield :: f p -> (T.Text, T.Text, [T.Text])

  gmapsTo :: f p -> [(T.Text, SQLValue)]
  gmapsFrom :: f p -> M.Map T.Text SQLValue -> f p

class GSelector f where
  gattrs :: f p -> (T.Text, [T.Text])
  gmapTo :: f p -> SQLValue
  gmapFrom :: SQLValue -> f p

instance (Datatype d, GMapper t) => GMapper (D1 d t) where
  grecord (x :: D1 d t p) = (T.pack $ datatypeName (undefined :: M1 _i d _f _p), gfields (unM1 x))

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
  gfield s = let (ft,attrs) = gattrs (unM1 s) in (T.pack $ selName s, ft, attrs)

  gmapsTo r = [(T.pack $ selName r, gmapTo (unM1 r))]
  gmapsFrom r mp = maybe r (M1 . gmapFrom) (mp M.!? T.pack (selName r))

instance (Mapper attrs, SQLField t) => GSelector (Rec0 (t :- attrs)) where
  gattrs (x :: Rec0 (t :- attrs) p) = (T.pack $ fieldType (undefined :: t), attrs (Proxy :: Proxy attrs))
  gmapTo = encode . getField . unK1
  gmapFrom = K1 . Field . decode

instance {-# OVERLAPS #-} SQLField r => GSelector (Rec0 r) where
  gattrs (x :: Rec0 r p) = (T.pack $ fieldType (undefined :: r), [])
  gmapTo = encode . unK1
  gmapFrom = K1 . decode

class Mapper a where
  attrs :: Proxy a -> [T.Text]

instance Mapper '[] where
  attrs Proxy = []

instance (Mapper xs, KnownSymbol x) => Mapper (x : xs) where
  attrs (Proxy :: Proxy (x:xs)) = T.pack (symbolVal (Proxy :: Proxy x)) : attrs (Proxy :: Proxy xs)

type RMapper a = (Generic a, GMapper (Rep a))

mapToSQLValues :: RMapper a => a -> [(T.Text, SQLValue)]
mapToSQLValues = gmapsTo . from

mapFromSQLValues :: RMapper a => a -> M.Map T.Text SQLValue -> a
mapFromSQLValues r = to . gmapsFrom (from r)

recordTypeOf :: RMapper a => a -> (T.Text, M.Map T.Text (T.Text, [T.Text]))
recordTypeOf =
  (\(x, y) -> (x, M.fromList $ map (\(a, b, c) -> (a, (b, c))) y))
    . grecord
    . from
