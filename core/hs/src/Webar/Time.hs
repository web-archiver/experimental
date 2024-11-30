{-# LANGUAGE LambdaCase #-}

module Webar.Time
  ( Timestamp (tsSecs, tsNanos),
    mkTestTimestamp,
    TimePeriod (..),
  )
where

import qualified Codec.CBOR.Decoding as Dec
import qualified Codec.CBOR.Encoding as Enc
import Data.Word (Word32, Word64)
import Webar.Codec.GCbor.Internal.Decoding
import Webar.Codec.GCbor.Internal.Encoding

encodeMap2 :: (Int, Enc.Encoding) -> (Int, Enc.Encoding) -> Enc.Encoding
encodeMap2 (k0, v0) (k1, v1) =
  Enc.encodeMapLen 2
    <> (Enc.encodeInt k0 <> v0)
    <> (Enc.encodeInt k1 <> v1)

encodeMap3 :: (Int, Enc.Encoding) -> (Int, Enc.Encoding) -> (Int, Enc.Encoding) -> Enc.Encoding
encodeMap3 (k0, v0) (k1, v1) (k2, v2) =
  Enc.encodeMapLen 3
    <> (Enc.encodeInt k0 <> v0)
    <> (Enc.encodeInt k1 <> v1)
    <> (Enc.encodeInt k2 <> v2)

encodeMap4 ::
  (Int, Enc.Encoding) ->
  (Int, Enc.Encoding) ->
  (Int, Enc.Encoding) ->
  (Int, Enc.Encoding) ->
  Enc.Encoding
encodeMap4 (k0, v0) (k1, v1) (k2, v2) (k3, v3) =
  Enc.encodeMapLen 4
    <> (Enc.encodeInt k0 <> v0)
    <> (Enc.encodeInt k1 <> v1)
    <> (Enc.encodeInt k2 <> v2)
    <> (Enc.encodeInt k3 <> v3)

(.:) :: Int -> Dec.Decoder s b -> Dec.Decoder s b
k .: v =
  Dec.decodeIntCanonical >>= \i ->
    if k == i
      then v
      else fail "Unexpected key"

data UncertaintyTime = UncertaintyTime
  { utSecs :: {-# UNPACK #-} Word64,
    utNanos :: {-# UNPACK #-} Word32
  }
  deriving (Show, Eq)

data TimeUncertainty
  = TuUnknown
  | TuTime {-# UNPACK #-} UncertaintyTime
  deriving (Show, Eq)

data Timescale
  = TsUtc
  deriving (Show, Eq)

-- | Time based on [rfc9581](https://www.rfc-editor.org/rfc/rfc9581.html)
data Timestamp = Timestamp
  { tsSecs :: {-# UNPACK #-} Word64,
    tsNanos :: {-# UNPACK #-} Word32,
    tsTimescale :: {-# UNPACK #-} Timescale,
    tsUncertainty :: {-# UNPACK #-} TimeUncertainty
  }
  deriving (Show, Eq)

-- | for test only
mkTestTimestamp :: Word64 -> Word32 -> Maybe (Word64, Word32) -> Timestamp
mkTestTimestamp s n u =
  Timestamp
    { tsSecs = s,
      tsNanos = n,
      tsTimescale = TsUtc,
      tsUncertainty = case u of
        Just (us, un) -> TuTime UncertaintyTime {utSecs = us, utNanos = un}
        Nothing -> TuUnknown
    }

instance ToGCbor Timestamp where
  toGCbor ts =
    Encoding
      ( Enc.encodeTag 1001 <> case tsUncertainty ts of
          TuUnknown ->
            encodeMap3
              (1, Enc.encodeWord64 (tsSecs ts))
              (13, Enc.encodeWord8 0) -- utc
              (-9, Enc.encodeWord32 (tsNanos ts))
          TuTime u ->
            encodeMap4
              (1, Enc.encodeWord64 (tsSecs ts))
              (13, Enc.encodeWord8 0) -- utc
              ( -7,
                encodeMap2
                  (1, Enc.encodeWord64 (utSecs u))
                  (-9, Enc.encodeWord32 (utNanos u))
              )
              (-9, Enc.encodeWord32 (tsNanos ts))
      )

instance FromGCbor Timestamp where
  fromGCbor =
    Decoder
      ( do
          Dec.decodeTagCanonical >>= \case
            1001 -> pure ()
            _ -> fail "Timestamp: invalid tag"
          Dec.decodeMapLenCanonical >>= \case
            3 -> do
              secs <- 1 .: Dec.decodeWord64Canonical
              scale <- timescaleField
              nanos <- (-9) .: Dec.decodeWord32Canonical
              pure
                Timestamp
                  { tsSecs = secs,
                    tsNanos = nanos,
                    tsTimescale = scale,
                    tsUncertainty = TuUnknown
                  }
            4 -> do
              secs <- 1 .: Dec.decodeWord64Canonical
              scale <- timescaleField
              uncert <- (-7) .: uncertaintyTime
              nanos <- (-9) .: Dec.decodeWord32Canonical
              pure
                Timestamp
                  { tsSecs = secs,
                    tsNanos = nanos,
                    tsTimescale = scale,
                    tsUncertainty = TuTime uncert
                  }
            _ -> fail "Timestamp: invalid map len"
      )
    where
      timescaleField :: Dec.Decoder s Timescale
      timescaleField =
        (13 .: Dec.decodeWord8Canonical) >>= \case
          0 -> pure TsUtc
          _ -> fail "Timestamp: unknown timescale"

      uncertaintyTime =
        Dec.decodeMapLenCanonical >>= \case
          2 -> do
            secs <- 1 .: Dec.decodeWord64Canonical
            nanos <- (-9) .: Dec.decodeWord32Canonical
            pure UncertaintyTime {utSecs = secs, utNanos = nanos}
          _ -> fail "UncertaintyTime: invalid map size"

-- | Period based on [rfc9581](https://www.rfc-editor.org/rfc/rfc9581.html)
data TimePeriod
  = TimePeriod
      {-# UNPACK #-} Timestamp
      {-# UNPACK #-} Timestamp
  deriving (Show, Eq)

instance ToGCbor TimePeriod where
  toGCbor (TimePeriod t0 t1) =
    Encoding (Enc.encodeTag 1003 <> Enc.encodeListLen 2)
      <> toGCbor t0
      <> toGCbor t1

instance FromGCbor TimePeriod where
  fromGCbor =
    Decoder
      ( do
          Dec.decodeTagCanonical >>= \case
            1003 -> pure ()
            _ -> fail "TimePeriod: invalid tag"
          Dec.decodeListLenCanonicalOf 2
      )
      >> TimePeriod <$> fromGCbor <*> fromGCbor