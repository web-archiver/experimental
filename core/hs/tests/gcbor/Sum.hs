{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Sum (spec) where

import Common (mkTest)
import Data.Int
import qualified Data.Vector as V
import Data.Word
import Test.Hspec
import Webar.Codec.GCbor.TH
import Webar.Text.Normalized (NFText, nfTxt)

data SUnit
  = SuA
  | SuB
  | SuVar3
  deriving (Show, Eq)

deriveSumGCbor
  (simpleSumOptions (toSnakeCase 2) mkEncodedName [])
  ''SUnit

unit :: Spec
unit = describe "unit" do
  mkTest "a" SuA "var_a"
  mkTest "b" SuB "var_b"
  mkTest "var3" SuVar3 "var_var3"

data SUnary
  = StUv Bool
  | StUv2 (V.Vector Word32)
  deriving (Show, Eq)

deriveSumGCbor
  (simpleSumOptions (toSnakeCase 2) mkEncodedName [])
  ''SUnary

unary :: Spec
unary = describe "unary" do
  mkTest "uv" (StUv True) "var_uv"
  mkTest "uv2" (StUv2 (V.fromList [123, 456])) "var_uv2"

data SNormal
  = SnNv1 Word8 Int16
  | SnNv2 Bool NFText
  deriving (Show, Eq)

deriveSumGCbor
  (simpleSumOptions (toSnakeCase 2) mkEncodedName [])
  ''SNormal

normal :: Spec
normal = describe "normal" do
  mkTest "nv1" (SnNv1 123 (-3200)) "var_nv1"
  mkTest "nv2" (SnNv2 False [nfTxt|123|]) "var_nv2"

data SRecord
  = SrRV1
      { srA :: Word32,
        srK :: Int16,
        srAb :: Int8,
        srC :: Bool
      }
  | SrRv2
      { srZ :: Bool,
        srFull :: Bool,
        srNew :: Word32
      }
  deriving (Show, Eq)

deriveSumGCbor
  (simpleSumOptions (toSnakeCase 2) (toSnakeCase 2) [])
  ''SRecord

record :: Spec
record = describe "record" do
  mkTest "rv1" SrRV1 {srA = 10, srK = -128, srAb = -10, srC = False} "var_rv1"
  mkTest "rv2" SrRv2 {srZ = False, srFull = True, srNew = 0} "var_rv2"

data SMixed
  = MvUnit
  | MvU2
  | MvUnary Int8
  | MvNormal Bool Bool
  | MvRecord
      { mvsC :: Int8,
        mvsAc :: Bool,
        mvsAb :: NFText
      }
  deriving (Show, Eq)

deriveSumGCbor
  (simpleSumOptions (toSnakeCase 0) (toSnakeCase 3) [])
  ''SMixed

mixed :: Spec
mixed = describe "mixed" do
  mkTest "unit" MvUnit "var_mv_unit"
  mkTest "u2" MvU2 "var_mv_u2"
  mkTest "unary" (MvUnary (-10)) "var_mv_unary"
  mkTest "normal" (MvNormal False True) "var_mv_normal"
  mkTest "record" MvRecord {mvsC = -1, mvsAc = True, mvsAb = [nfTxt|a|]} "var_mv_record"

spec :: Spec
spec = describe "sum" do
  unit
  unary
  normal
  record
  mixed