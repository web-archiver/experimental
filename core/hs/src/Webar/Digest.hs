{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Webar.Digest
  ( Sha256,
    sha256FromByteString,
    sha256ToByteString,
    sha256QQ,
    Digest (..),
    hashBytes,
    hashHandle,
  )
where

import qualified Codec.CBOR.Decoding as Dec
import qualified Codec.CBOR.Encoding as Enc
import qualified Crypto.Hash as H
import qualified Crypto.Hash.IO as H.IO
import Data.ByteArray ()
import qualified Data.ByteArray as BA
import qualified Data.ByteArray.Encoding as BAE
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.C
import Data.Primitive.ByteArray
  ( newPinnedByteArray,
    withMutableByteArrayContents,
  )
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import System.IO
import Webar.Codec.GCbor.Internal.Decoding
import Webar.Codec.GCbor.Internal.Encoding
import Webar.Codec.GCbor.Internal.Ord

newtype Sha256 = Sha256 BS.ByteString
  deriving (Show, Eq, Lift)

instance GCborOrd Sha256 where
  compareGCbor (Sha256 l) (Sha256 r) = compare l r

instance ToGCbor Sha256 where
  toGCbor (Sha256 h) = Encoding (Enc.encodeTag 18540 <> Enc.encodeBytes (BA.convert h))

instance FromGCbor Sha256 where
  fromGCbor =
    Decoder
      ( Dec.decodeTagCanonical >>= \case
          18540 ->
            Dec.decodeBytesCanonical >>= \bs ->
              if BS.length bs == 32
                then pure (Sha256 bs)
                else fail "Sha256: size mismatch"
          _ -> fail "Sha256: expect sha256 tag 18540"
      )

sha256FromByteString :: BS.ByteString -> Maybe Sha256
sha256FromByteString bs
  | BS.length bs == 32 = Just (Sha256 bs)
  | otherwise = Nothing

sha256ToByteString :: Sha256 -> BS.ByteString
sha256ToByteString (Sha256 d) = d

sha256QQ :: QuasiQuoter
sha256QQ =
  QuasiQuoter
    { quoteExp = \s -> decodeBase16 s >>= lift,
      quotePat = \_ -> fail "pat quote is not supported",
      quoteDec = \_ -> fail "dec quote is not supported",
      quoteType = \_ -> fail "type quote is not supported"
    }
  where
    decodeBase16 s = do
      bs <- case BAE.convertFromBase BAE.Base16 (BS.C.pack s) of
        Right v -> pure v
        Left e -> fail ("invalid sha256 hex: " ++ e)
      if BS.length bs == 32
        then pure (Sha256 bs)
        else fail "size mismatch"

newtype Digest = DSha256 Sha256
  deriving (Show, Eq, GCborOrd, ToGCbor, FromGCbor, Lift)

hashBytes :: BS.ByteString -> Digest
hashBytes b = DSha256 (Sha256 (BA.convert (H.hashWith H.SHA256 b)))

hashHandle :: Handle -> IO Digest
hashHandle h = do
  ctx <- H.IO.hashMutableInitWith H.SHA256
  buf <- newPinnedByteArray bufSize
  doHash ctx buf
  where
    bufSize = 16 * 1024

    doHash ctx buf = do
      l <- withMutableByteArrayContents buf (\ptr -> hGetBuf h ptr bufSize)
      if l == 0
        then DSha256 . Sha256 . BA.convert <$> H.IO.hashMutableFinalize ctx
        else
          withMutableByteArrayContents
            buf
            (\ptr -> H.IO.hashMutableUpdate ctx (BA.MemView ptr l))
            >> doHash ctx buf
