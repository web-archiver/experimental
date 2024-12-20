{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import qualified Data.ByteString as BS
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Typeable
import qualified Data.UUID.Types as UUID
import Data.Word (Word16, Word32, Word64, Word8)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Webar.Codec.GCbor
import Webar.Digest
import qualified Webar.Text.Normalized as NF
import Webar.Text.RawUtf8 (rawUtf8QQ)
import qualified Webar.Text.RawUtf8 as RawUtf8

check :: (GCborOrd a, ToGCbor a) => a -> a -> Expectation
check v1 v2 =
  compareGCbor v1 v2
    `shouldBe` compare
      (encodeStrictBs v1)
      (encodeStrictBs v2)

propTest :: forall a v. (Show a, Arbitrary a, Typeable v, ToGCbor v, GCborOrd v) => (a -> v) -> Spec
propTest f =
  prop (show (typeRep (Proxy :: Proxy v))) \(a1, a2) -> check (f a1) (f a2)

fixedTest :: (GCborOrd a, ToGCbor a) => String -> a -> a -> Spec
fixedTest name v1 v2 = it name (check v1 v2)

newtype Sha256Val = Sha256Val {getSha256Val :: Sha256}
  deriving (Show)

instance Arbitrary Sha256Val where
  arbitrary =
    fmap
      (Sha256Val . fromJust . sha256FromByteString . BS.pack)
      (vectorOf 32 arbitrary)

rawUtf8Txt :: Spec
rawUtf8Txt = describe "RawUtf8" do
  let sAbc = [rawUtf8QQ|abc|]
      sBa = [rawUtf8QQ|ba|]
      sEmpty = [rawUtf8QQ||]
      sA0308 = fromJust (RawUtf8.fromText "a\x0308")
      sE0323_0302 = fromJust (RawUtf8.fromText "e\x0323\x0302")
  fixedTest "nn_abc_empty" sAbc sEmpty
  fixedTest "nn_abc_ba" sAbc sBa
  fixedTest "un_abc_a" sAbc sA0308
  fixedTest "nu_e_ba" sE0323_0302 sBa
  fixedTest "uu_a_e" sA0308 sE0323_0302

main :: IO ()
main = hspec do
  describe "text" do
    propTest (\(ASCIIString s) -> fromJust (NF.fromAscii (T.pack s)))
    rawUtf8Txt
  describe "Int" do
    propTest @Int8 id
    fixedTest @Int8 "negative_overflow" (-128) (-1)

    propTest @Int16 id
    propTest @Int32 id
    propTest @Int64 id
  describe "Word" do
    propTest @Word8 id
    propTest @Word16 id
    propTest @Word32 id
    propTest @Word64 id
  propTest (uncurry UUID.fromWords64)
  describe "Bool" do
    fixedTest "false_false" False False
    fixedTest "false_true" False True
    fixedTest "true_false" True False
    fixedTest "true_true" True True
  fixedTest "Unit" () ()
  describe "digest" do
    propTest getSha256Val
    propTest (DSha256 . getSha256Val)
