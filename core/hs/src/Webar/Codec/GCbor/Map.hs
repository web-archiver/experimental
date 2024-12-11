{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Webar.Codec.GCbor.Map
  ( GCborMap,
    empty,
    null,
    size,
    member,
    lookup,
    insert,
    delete,
    fromList,
  )
where

import qualified Codec.CBOR.Decoding as Dec
import qualified Codec.CBOR.Encoding as Enc
import Data.Coerce (coerce)
import qualified Data.Map.Strict as M
import Webar.Codec.GCbor.Internal.Decoding
import Webar.Codec.GCbor.Internal.Encoding
import Webar.Codec.GCbor.Internal.Ord (GCborOrd)
import Webar.Codec.GCbor.Key
import Prelude hiding (lookup, null)

newtype GCborMap k v = GCborMap (M.Map (GCborKey k) v)
  deriving (Show, Eq)

empty :: GCborMap k v
empty = GCborMap M.empty

null :: GCborMap k a -> Bool
null (GCborMap m) = M.null m

size :: GCborMap k a -> Int
size (GCborMap m) = M.size m

member :: (GCborOrd k) => k -> GCborMap k a -> Bool
member k (GCborMap m) = M.member (GCborKey k) m

lookup :: (GCborOrd k) => k -> GCborMap k a -> Maybe a
lookup k (GCborMap m) = M.lookup (GCborKey k) m

insert :: (GCborOrd k) => k -> v -> GCborMap k v -> GCborMap k v
insert k v (GCborMap m) = GCborMap (M.insert (GCborKey k) v m)

delete :: (GCborOrd k) => k -> GCborMap k v -> GCborMap k v
delete k (GCborMap m) = GCborMap (M.delete (GCborKey k) m)

fromList :: (GCborOrd k) => [(k, v)] -> GCborMap k v
fromList l = GCborMap (M.fromList (coerce l))

instance (ToGCbor k, ToGCbor v) => ToGCbor (GCborMap k v) where
  toGCbor (GCborMap m) =
    M.foldlWithKey'
      (\e (GCborKey k) v -> e <> toGCbor k <> toGCbor v)
      (Encoding (Enc.encodeTag 259 <> Enc.encodeMapLen (fromIntegral (M.size m))))
      m

instance (GCborOrd k, FromGCbor k, FromGCbor v) => FromGCbor (GCborMap k v) where
  fromGCbor =
    Decoder
      ( Dec.decodeTagCanonical >>= \case
          259 ->
            Dec.decodeMapLenCanonical >>= \l ->
              Dec.decodeSequenceLenN
                (\m (k, v) -> insert k v m)
                empty
                id
                l
                (liftA2 (,) (getDecoder fromGCbor) (getDecoder fromGCbor))
          _ -> fail "GCborMap: expect tag 259"
      )