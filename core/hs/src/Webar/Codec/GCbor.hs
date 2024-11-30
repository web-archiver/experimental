-- | Codec for graph cbor
--
-- Cbor encoder and decoder conform to
-- [rfc8949](https://datatracker.ietf.org/doc/html/rfc8949#name-deterministically-encoded-c)
-- and [dcbor](https://datatracker.ietf.org/doc/html/draft-mcnally-deterministic-cbor-11)
--
--  Encoder and decoder api is experimental and should use TH instead.
--
--  Encoder and decoder may split into a separate library when stabilized. Now we
--  put them in core so that types that need manual encode and decode implementation
--  can be handled. For other types that need manual encode and decode, define them
--  here and reexport in the corresponding package as temporary measure.
module Webar.Codec.GCbor
  ( ToGCbor,
    FromGCbor,
    -- only export basic functions now
    encodeStrictBs,
    DecodeError,
    decodeStrictBsThrow,
  )
where

import Codec.CBOR.Read
import Codec.CBOR.Write
import Control.Exception (Exception, throw)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Webar.Codec.GCbor.Internal.Decoding
import Webar.Codec.GCbor.Internal.Encoding

encodeStrictBs :: (ToGCbor a) => a -> BS.ByteString
encodeStrictBs v = toStrictByteString (getEncoding (toGCbor v))

data DecodeError
  = DeserializeError DeserialiseFailure
  | RemainingInputs LBS.ByteString
  deriving (Show)

instance Exception DecodeError

fromResult :: Either DeserialiseFailure (LBS.ByteString, a) -> Either DecodeError a
fromResult (Right (bs, v))
  | LBS.null bs = Right v
  | otherwise = Left (RemainingInputs bs)
fromResult (Left e) = Left (DeserializeError e)

decodeLazyBs :: (FromGCbor a) => LBS.ByteString -> Either DecodeError a
decodeLazyBs bs = fromResult (deserialiseFromBytes (getDecoder fromGCbor) bs)

decodeStrictBs :: (FromGCbor a) => BS.ByteString -> Either DecodeError a
decodeStrictBs = decodeLazyBs . LBS.fromStrict

decodeStrictBsThrow :: (FromGCbor a) => BS.ByteString -> a
decodeStrictBsThrow bs = case decodeStrictBs bs of
  Right r -> r
  Left e -> throw e
