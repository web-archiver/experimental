{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Webar.Codec.GCbor.Internal.Decoding
  ( Decoder (..),
    MaybeIndex (..),
    RecordState,
    RecordResult (..),
    decodeRequiredField,
    decodeOmissibleField,
    decodeNormalProductOf,
    decodeRecordProduct,
    SumKind (..),
    decodeSum,
    decodeUnarySum,
    decodeNormalSumOf,
    decodeRecordSum,
    FromGCbor (..),
  )
where

import qualified Codec.CBOR.Decoding as Dec
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Coerce (coerce)
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

data MaybeIndex
  = Known {-# UNPACK #-} Word
  | Unknown

data RecordState = RecordState
  { typeName :: ~String,
    currentIndex :: {-# UNPACK #-} Word,
    remaining :: {-# UNPACK #-} Word
  }

data RecordResult v = RecordResult v RecordState

nextFieldKey :: (NT.NFText -> MaybeIndex) -> RecordState -> Decoder s RecordState
nextFieldKey _ state@(RecordState _ _ 0) = pure state {currentIndex = maxBound}
nextFieldKey f (RecordState ty lastIdx remain) =
  fromGCbor >>= \v -> case f v of
    Known idx
      | idx > lastIdx ->
          pure
            RecordState
              { typeName = ty,
                currentIndex = idx,
                remaining = remain - 1
              }
      | otherwise -> fail (ty ++ ": Unexpected field " ++ show v)
    Unknown -> fail (ty ++ ": Unexpected field " ++ show v)

decodeRequiredField ::
  (FromGCbor v) =>
  (NT.NFText -> MaybeIndex) ->
  Word ->
  RecordState ->
  Decoder s (RecordResult v)
decodeRequiredField f idx state@(RecordState ty current _)
  | idx == current = RecordResult <$> fromGCbor <*> nextFieldKey f state
  | otherwise = fail (ty ++ ": Missing required field")

decodeOmissibleField ::
  (FromGCbor v) =>
  (NT.NFText -> MaybeIndex) ->
  Word ->
  RecordState ->
  Decoder s (RecordResult (Maybe v))
decodeOmissibleField f idx state@(RecordState _ current _) =
  if idx < current
    then pure (RecordResult Nothing state)
    else -- idx == current
      RecordResult <$> fmap Just fromGCbor <*> nextFieldKey f state

decodeNormalProductOf :: Int -> Decoder s ()
decodeNormalProductOf = coerce Dec.decodeListLenCanonicalOf

startRecord :: String -> (NT.NFText -> MaybeIndex) -> Int -> Decoder s RecordState
startRecord ty _ 0 =
  pure
    RecordState
      { typeName = ty,
        currentIndex = maxBound,
        remaining = 0
      }
startRecord ty f n =
  fromGCbor >>= \name -> case f name of
    Known idx ->
      pure
        RecordState
          { typeName = ty,
            currentIndex = idx,
            remaining = fromIntegral (n - 1)
          }
    Unknown -> fail (ty ++ ": Unknown field " ++ show name)

decodeRecordProduct ::
  String ->
  (NT.NFText -> MaybeIndex) ->
  (RecordState -> Decoder s (RecordResult v)) ->
  Decoder s v
decodeRecordProduct ty f body =
  Decoder Dec.decodeMapLenCanonical
    >>= startRecord ty f
    >>= body
    >>= \(RecordResult v finalState) ->
      case finalState of
        RecordState {remaining = 0} -> pure v
        RecordState {remaining = r} ->
          fail
            ( ty
                ++ ": Serialized record contains "
                ++ show r
                ++ " fields"
            )

data SumKind = SkUnit NT.NFText | SkCompound NT.NFText

decodeSum :: String -> Decoder s SumKind
decodeSum ty =
  Decoder
    ( Dec.peekTokenType >>= \case
        Dec.TypeString -> SkUnit <$> getDecoder fromGCbor
        Dec.TypeTag -> do
          Dec.decodeTagCanonical >>= \case
            27 -> pure ()
            _ -> fail (ty ++ ": invalid tag, expect tag 27")
          Dec.decodeListLenCanonicalOf 2
          t <- getDecoder fromGCbor
          pure (SkCompound t)
        _ -> fail (ty ++ ": invalid sum, expect string or tag 27")
    )

decodeUnarySum :: (FromGCbor v) => Decoder s v
decodeUnarySum = fromGCbor

decodeNormalSumOf :: Int -> Decoder s ()
decodeNormalSumOf = decodeNormalProductOf

decodeRecordSum ::
  String ->
  (NT.NFText -> MaybeIndex) ->
  (RecordState -> Decoder s (RecordResult v)) ->
  Decoder s v
decodeRecordSum = decodeRecordProduct