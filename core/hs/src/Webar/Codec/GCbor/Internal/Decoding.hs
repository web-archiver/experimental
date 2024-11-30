{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Webar.Codec.GCbor.Internal.Decoding
  ( Decoder (..),
    FromGCbor (..),
  )
where

import qualified Codec.CBOR.Decoding as Dec
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as V
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import qualified Webar.Text.Normalized as NT

newtype Decoder s a = Decoder {getDecoder :: Dec.Decoder s a}
  deriving (Functor, Applicative, Monad, MonadFail)

class FromGCbor a where
  fromGCbor :: Decoder s a

instance FromGCbor Word8 where
  fromGCbor = Decoder Dec.decodeWord8Canonical

instance FromGCbor Word16 where
  fromGCbor = Decoder Dec.decodeWord16Canonical

instance FromGCbor Word32 where
  fromGCbor = Decoder Dec.decodeWord32Canonical

instance FromGCbor Word64 where
  fromGCbor = Decoder Dec.decodeWord64Canonical

instance FromGCbor Word where
  fromGCbor = Decoder Dec.decodeWordCanonical

instance FromGCbor Int8 where
  fromGCbor = Decoder Dec.decodeInt8Canonical

instance FromGCbor Int16 where
  fromGCbor = Decoder Dec.decodeInt16Canonical

instance FromGCbor Int32 where
  fromGCbor = Decoder Dec.decodeInt32Canonical

instance FromGCbor Int64 where
  fromGCbor = Decoder Dec.decodeInt64Canonical

instance FromGCbor Int where
  fromGCbor = Decoder Dec.decodeIntCanonical

instance FromGCbor () where
  fromGCbor = Decoder Dec.decodeNull

instance FromGCbor Void where
  fromGCbor = fail "Void"

instance FromGCbor Bool where
  fromGCbor = Decoder Dec.decodeBool

instance FromGCbor NT.NFText where
  fromGCbor =
    Decoder
      ( Dec.decodeStringCanonical >>= \s ->
          case NT.fromAscii s of
            Just r -> pure r
            Nothing -> fail "NFText: Invalid text"
      )

instance FromGCbor ByteString where
  fromGCbor = Decoder Dec.decodeBytesCanonical

instance (FromGCbor a) => FromGCbor (V.Vector a) where
  fromGCbor =
    Decoder
      ( Dec.decodeListLenCanonical >>= \l ->
          Dec.decodeSequenceLenN
            (\revList i -> i : revList)
            []
            (\revList -> V.fromListN l (reverse revList))
            l
            (getDecoder fromGCbor)
      )

instance FromGCbor UUID.UUID where
  fromGCbor =
    Decoder
      ( Dec.decodeTagCanonical >>= \case
          37 ->
            Dec.decodeBytesCanonical >>= \b ->
              case UUID.fromByteString (LBS.fromStrict b) of
                Just u -> pure u
                Nothing -> fail "Expect uuid bytestring"
          _ -> fail "Expect uuid tag 37"
      )
