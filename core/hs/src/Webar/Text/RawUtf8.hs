{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}

module Webar.Text.RawUtf8
  ( RawUtf8Text,
    fromText,
    toText,
    rawUtf8QQ,
  )
where

import qualified Codec.CBOR.Decoding as Dec
import qualified Codec.CBOR.Encoding as Enc
import qualified Codec.CBOR.Read as Cbor.Read
import qualified Codec.CBOR.Write as Cbor.Write
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Normalize as T.N
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lift (lift))
import qualified Unicode.Char.General as Ch
import Webar.Codec.GCbor.Internal.Decoding
import Webar.Codec.GCbor.Internal.Encoding
import Webar.Codec.GCbor.Internal.Ord

-- | Raw utf8 text from upstream that may be not normalized but does not
-- contains unassigned character
data RawUtf8Text = RawUtf8Text
  { normalized :: {-# UNPACK #-} Bool,
    text :: T.Text
  }
  deriving (Show, Eq, Lift)

isNormalized :: T.Text -> Bool
isNormalized t = T.N.normalize T.N.NFC t == t

noUnassigned :: T.Text -> Bool
noUnassigned = not . T.any (\c -> Ch.generalCategory c == Ch.NotAssigned)

fromText :: T.Text -> Maybe RawUtf8Text
fromText t
  | noUnassigned t = Just (RawUtf8Text (isNormalized t) t)
  | otherwise = Nothing

toText :: RawUtf8Text -> T.Text
toText = text

rawUtf8QQ :: QuasiQuoter
rawUtf8QQ =
  QuasiQuoter
    { quoteExp = \s -> case fromText (T.pack s) of
        Just v -> lift v
        Nothing -> fail "Contains unassigned character",
      quotePat = \_ -> fail "pat quoting is not supported",
      quoteDec = \_ -> fail "Dec quoting is not supported",
      quoteType = \_ -> fail "Type quoting is not supported"
    }

instance GCborOrd RawUtf8Text where
  compareGCbor (RawUtf8Text True l) (RawUtf8Text True r) =
    compareText l r
  compareGCbor (RawUtf8Text True _) (RawUtf8Text False _) = LT
  compareGCbor (RawUtf8Text False _) (RawUtf8Text True _) = GT
  compareGCbor (RawUtf8Text False l) (RawUtf8Text False r) =
    compareText l r

instance ToGCbor RawUtf8Text where
  toGCbor RawUtf8Text {normalized = True, text = t} = Encoding (Enc.encodeString t)
  toGCbor RawUtf8Text {normalized = False, text = t} =
    Encoding
      ( Enc.encodeTag 24
          <> Enc.encodeBytes (Cbor.Write.toStrictByteString (Enc.encodeString t))
      )

instance FromGCbor RawUtf8Text where
  fromGCbor =
    Decoder
      ( Dec.peekTokenType >>= \case
          Dec.TypeString ->
            Dec.decodeStringCanonical >>= \t ->
              checkUnassigned t
                >> if isNormalized t
                  then pure RawUtf8Text {normalized = True, text = t}
                  else fail "RawUtf8Text: string is encoded as normalized but not normalized"
          Dec.TypeTag ->
            Dec.decodeTagCanonical >>= \case
              24 ->
                Dec.decodeBytesCanonical >>= \bs ->
                  case Cbor.Read.deserialiseFromBytes Dec.decodeStringCanonical (LBS.fromStrict bs) of
                    Right (r, v)
                      | LBS.null r ->
                          checkUnassigned v
                            >> if isNormalized v
                              then fail "RawUtf8Text: string is encoded as not normalized but is normalized"
                              else pure RawUtf8Text {normalized = False, text = v}
                      | otherwise -> fail "RawUtf8Text: trailing bytes in encoding"
                    Left err -> fail ("RawUtf8Text: failed to decode bytes " ++ show err)
              _ -> fail "RawUtf8Text: tag 24"
          _ -> fail "RawUtf8Text: expect string or tag 24"
      )
    where
      checkUnassigned :: T.Text -> Dec.Decoder s ()
      checkUnassigned t
        | noUnassigned t = pure ()
        | otherwise = fail ("RawUtf8Text: " ++ show t ++ " contains unassigned character")
