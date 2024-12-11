module Webar.Codec.GCbor.Key (GCborKey (..)) where

import Webar.Codec.GCbor.Internal.Ord

newtype GCborKey k = GCborKey {getGCborKey :: k}
  deriving (Show, Eq)

instance (GCborOrd k) => Ord (GCborKey k) where
  compare (GCborKey l) (GCborKey r) = compareGCbor l r
