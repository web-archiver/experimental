{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Bits (FiniteBits (finiteBitSize))
import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Maybe (fromJust)
import Data.Proxy
import qualified Data.Text.Encoding as TE
import Data.Typeable (Typeable, typeOf, typeRep)
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as V
import Data.Word (Word16, Word32, Word64, Word8)
import System.FilePath ((<.>), (</>))
import Test.Hspec
import Webar.Codec.GCbor
import Webar.Text.Normalized (NFText, nfTxt)
import qualified Webar.Text.Normalized as NF

mkTestBin :: (Show a, Eq a, ToGCbor a, FromGCbor a) => String -> a -> BS.ByteString -> Spec
mkTestBin name v bin =
  describe name do
    it "serialize" (encodeStrictBs v `shouldBe` bin)
    it "deserialize" do
      decodeStrictBsThrow bin `shouldBe` v

mkTest :: (Show a, Eq a, FromGCbor a, ToGCbor a) => String -> a -> FilePath -> Spec
mkTest name v path = do
  bin <- runIO (BS.readFile ("tests/gcbor/data" </> path <.> "bin"))
  mkTestBin name v bin

minMax :: Spec
minMax = describe "min_max" do
  testInt @Int64 Proxy
  testWord @Word64 Proxy
  testInt @Int8 Proxy
  testWord @Word8 Proxy
  testInt @Int16 Proxy
  testWord @Word16 Proxy
  testInt @Int32 Proxy
  testWord @Word32 Proxy
  where
    testInt ::
      forall a.
      (Show a, Eq a, Bounded a, FiniteBits a, ToGCbor a, FromGCbor a, Typeable a) =>
      Proxy a ->
      Spec
    testInt p = describe (show (typeRep p)) do
      let tName = "i" ++ show (finiteBitSize (minBound @a))
      mkTest "min" (minBound @a) (tName ++ "_min")
      mkTest "max" (maxBound @a) (tName ++ "_max")

    testWord ::
      forall a.
      (Show a, Eq a, Bounded a, FiniteBits a, ToGCbor a, FromGCbor a, Typeable a) =>
      Proxy a ->
      Spec
    testWord p =
      mkTest
        (show (typeRep p))
        (maxBound @a)
        ("u" ++ show (finiteBitSize (maxBound @a)) ++ "_max")

integer :: Spec
integer = describe "integer" do
  describe "positive" do
    testVal @Word8 0 "zero"
    testVal @Word64 0 "zero"
    testVal @Word32 1 "one"
    testVal @Int32 10 "ten"
    testVal @Word8 23 "23"
    testVal @Word64 24 "24"
    testVal @Int8 100 "100"
    testVal @Word16 1000 "1000"
    testVal @Word32 1_000_000 "million"
    testVal @Int64 1_000_000_000_000 "1e12"
  describe "negative" do
    testVal @Int8 (-1) "minus_one"
    testVal @Int16 (-10) "minus_ten"
    testVal @Int32 (-100) "-100"
    testVal @Int16 (-1000) "-1000"
  minMax
  where
    testVal ::
      (Show a, Eq a, ToGCbor a, FromGCbor a, Typeable a) =>
      a ->
      FilePath ->
      Spec
    testVal a = mkTest (show a ++ "::" ++ show (typeOf a)) a

text :: Spec
text = describe "text" do
  test [nfTxt||] "empty_str"
  test [nfTxt|a|] "a"
  test [nfTxt|IETF|] "ietf"
  test [nfTxt|"\|] "escape_str"

  large <- runIO (TE.decodeUtf8 <$> BS.readFile "tests/gcbor/data/large_text.txt")
  mkTest "large" (fromJust (NF.fromAscii large)) "large_text"
  where
    test :: NFText -> FilePath -> Spec
    test v = mkTest (show v) v

array :: Spec
array = describe "array" do
  mkTest "empty" (V.empty @Word8) "empty_array"
  mkTest "123" (V.fromList @Word32 [1, 2, 3]) "123_array"
  mkTest "25 elem" (V.fromList @Int32 [1 .. 25]) "array_25"

bytes :: Spec
bytes = describe "bytes" do
  mkTest "empty" BS.empty "empty_bytes"
  mkTest "sample" (BS.pack [0x01, 0x02, 0x03, 0x04]) "sample_bytes"

uuidTests :: Spec
uuidTests = describe "uuid" do
  mkTest "nil" UUID.nil "uuid_nil"
  mkTest "sample" (UUID.fromWords64 0xc2cc_10e1_57d6_4b6f 0x9899_38d9_7211_2d8c) "uuid_1"
  mkTest "max" (UUID.fromWords64 0xffff_ffff_ffff_ffff 0xffff_ffff_ffff_ffff) "uuid_max"
  -- from rfc 9562
  mkTest "v4" (UUID.fromWords64 0x9191_08f7_52d1_3320 0x5bac_f847_db41_48a8) "uuid_v4"
  mkTest "v7" (UUID.fromWords64 0x017f_22e2_79b0_7cc3 0x98c4_dc0c_0c07_398f) "uuid_v7"

main :: IO ()
main = hspec do
  mkTest "unit" () "null"
  integer
  text
  array
  bytes
  describe "bool" do
    mkTest "false" False "false"
    mkTest "true" True "true"
  uuidTests