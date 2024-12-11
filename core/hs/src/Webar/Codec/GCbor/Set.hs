{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Webar.Codec.GCbor.Set
  ( GCborSet,
    empty,
    null,
    size,
    member,
    insert,
    delete,
    fromList,
  )
where

import qualified Codec.CBOR.Decoding as Dec
import qualified Codec.CBOR.Encoding as Enc
import Data.Coerce (coerce)
import qualified Data.Set as S
import Webar.Codec.GCbor.Internal.Decoding
import Webar.Codec.GCbor.Internal.Encoding
import Webar.Codec.GCbor.Internal.Ord (GCborOrd)
import Webar.Codec.GCbor.Key
import Prelude hiding (null)

newtype GCborSet v = GCborSet (S.Set (GCborKey v))
  deriving (Show, Eq)

empty :: GCborSet v
empty = GCborSet S.empty

null :: GCborSet v -> Bool
null (GCborSet s) = S.null s

size :: GCborSet v -> Int
size (GCborSet s) = S.size s

member :: (GCborOrd k) => k -> GCborSet k -> Bool
member v (GCborSet s) = S.member (GCborKey v) s

insert :: (GCborOrd v) => v -> GCborSet v -> GCborSet v
insert v (GCborSet s) = GCborSet (S.insert (GCborKey v) s)

delete :: (GCborOrd v) => v -> GCborSet v -> GCborSet v
delete v (GCborSet s) = GCborSet (S.delete (GCborKey v) s)

fromList :: (GCborOrd v) => [v] -> GCborSet v
fromList l = GCborSet (S.fromList (coerce l))

instance (ToGCbor v) => ToGCbor (GCborSet v) where
  toGCbor (GCborSet s) =
    S.foldl'
      (\e (GCborKey v) -> e <> toGCbor v)
      (Encoding (Enc.encodeTag 258 <> Enc.encodeListLen (fromIntegral (S.size s))))
      s

instance (GCborOrd v, FromGCbor v) => FromGCbor (GCborSet v) where
  fromGCbor =
    Decoder
      ( Dec.decodeTagCanonical >>= \case
          258 ->
            Dec.decodeListLenCanonical >>= \l ->
              Dec.decodeSequenceLenN
                (\s v -> insert v s)
                empty
                id
                l
                (getDecoder fromGCbor)
          _ -> fail "GCborSet: expect tag 258"
      )
