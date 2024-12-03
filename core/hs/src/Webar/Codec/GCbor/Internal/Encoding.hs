{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Webar.Codec.GCbor.Internal.Encoding
  ( Encoding (..),
    omissibleFieldSize,
    encodeField,
    encodeOmissibleField,
    encodeNormalProduct,
    encodeRecordProduct,
    encodeUnitSum,
    encodeUnarySum,
    encodeNormalSum,
    encodeRecordSum,
    ToGCbor (..),
  )
where

import qualified Codec.CBOR.Encoding as Enc
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as V
import Data.Void (Void, absurd)
import Data.Word (Word16, Word32, Word64, Word8)
import qualified Webar.Text.Normalized as NT

newtype Encoding = Encoding {getEncoding :: Enc.Encoding}
  deriving (Semigroup, Monoid)

class ToGCbor a where
  toGCbor :: a -> Encoding

instance ToGCbor Word8 where
  toGCbor = coerce Enc.encodeWord8

instance ToGCbor Word16 where
  toGCbor = coerce Enc.encodeWord16

instance ToGCbor Word32 where
  toGCbor = coerce Enc.encodeWord32

instance ToGCbor Word64 where
  toGCbor = coerce Enc.encodeWord64

instance ToGCbor Word where
  toGCbor = coerce Enc.encodeWord

instance ToGCbor Int8 where
  toGCbor = coerce Enc.encodeInt8

instance ToGCbor Int16 where
  toGCbor = coerce Enc.encodeInt16

instance ToGCbor Int32 where
  toGCbor = coerce Enc.encodeInt32

instance ToGCbor Int64 where
  toGCbor = coerce Enc.encodeInt64

instance ToGCbor Int where
  toGCbor = coerce Enc.encodeInt

instance ToGCbor () where
  toGCbor _ = coerce Enc.encodeNull

instance ToGCbor Void where
  toGCbor = absurd

instance ToGCbor Bool where
  toGCbor = coerce Enc.encodeBool

instance ToGCbor NT.NFText where
  toGCbor t = Encoding (Enc.encodeString (NT.toText t))

instance ToGCbor ByteString where
  toGCbor = coerce Enc.encodeBytes

instance (ToGCbor a) => ToGCbor (V.Vector a) where
  toGCbor v =
    V.foldl'
      (\e a -> e <> toGCbor a)
      (Encoding (Enc.encodeListLen (fromIntegral (V.length v))))
      v

instance ToGCbor UUID.UUID where
  toGCbor u = Encoding (Enc.encodeTag 37 <> Enc.encodeBytes (LBS.toStrict (UUID.toByteString u)))

encodeField :: (ToGCbor v) => NT.NFText -> v -> Encoding
encodeField k v = toGCbor k <> toGCbor v

omissibleFieldSize :: Maybe a -> Word
omissibleFieldSize (Just _) = 1
omissibleFieldSize Nothing = 0
{-# INLINE omissibleFieldSize #-}

encodeOmissibleField :: (ToGCbor v) => NT.NFText -> Maybe v -> Encoding
encodeOmissibleField k (Just v) = toGCbor k <> toGCbor v
encodeOmissibleField _ Nothing = mempty

encodeNormalProduct :: Word -> Encoding
encodeNormalProduct = coerce Enc.encodeListLen

encodeRecordProduct :: Word -> Encoding
encodeRecordProduct = coerce Enc.encodeMapLen

encodeUnitSum :: NT.NFText -> Encoding
encodeUnitSum = toGCbor

compoundSum :: NT.NFText -> Encoding
compoundSum n = Encoding (Enc.encodeTag 27 <> Enc.encodeListLen 2) <> toGCbor n

encodeUnarySum :: (ToGCbor a) => NT.NFText -> a -> Encoding
encodeUnarySum n v = compoundSum n <> toGCbor v

encodeNormalSum :: NT.NFText -> Word -> Encoding
encodeNormalSum n l = compoundSum n <> Encoding (Enc.encodeListLen l)

encodeRecordSum :: NT.NFText -> Word -> Encoding
encodeRecordSum n l = compoundSum n <> Encoding (Enc.encodeMapLen l)