{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Product (spec) where

import Common (mkTest)
import Data.Int
import qualified Data.Vector as V
import Data.Word
import Test.Hspec
import Webar.Codec.GCbor (FromGCbor, ToGCbor)
import Webar.Codec.GCbor.TH
import Webar.Text.Normalized (NFText, nfTxt)

data PNormal = PNormal Int32 (V.Vector Int32) (V.Vector Word16)
  deriving (Show, Eq)

deriveProductGCbor defaultProductOptions ''PNormal

normal :: Spec
normal = describe "normal" do
  mkTest "0" (PNormal 1 (V.fromList [2, 3]) (V.fromList [4, 5])) "prod_normal_0"
  mkTest
    "bound"
    (PNormal minBound (V.fromList [0, maxBound, 10]) (V.fromList [1, 2, 3]))
    "prod_normal_bound"

data PRecord = PRecord
  { ssA :: Word32,
    ssB :: V.Vector Word64
  }
  deriving (Show, Eq)

deriveProductGCbor
  (simpleProductOptions (toSnakeCase 2) [])
  ''PRecord

record :: Spec
record = describe "record" do
  mkTest
    "0"
    PRecord {ssA = 1, ssB = V.fromList [2, 3]}
    "prod_record_0"
  mkTest
    "max"
    PRecord {ssA = maxBound, ssB = V.fromList [0, 1, maxBound]}
    "prod_record_max"

data PSort = PSort
  { psA :: Word8,
    psC :: V.Vector Int32,
    psAb :: Int32,
    psBac :: NFText
  }
  deriving (Show, Eq)

deriveProductGCbor
  (simpleProductOptions (toSnakeCase 2) [])
  ''PSort

pSort :: Spec
pSort = describe "sort" do
  mkTest
    "0"
    PSort
      { psC = V.fromList [-1, 0, 1],
        psBac = [nfTxt|example|],
        psAb = 10,
        psA = maxBound
      }
    "prod_sort_0"
  mkTest
    "1"
    PSort {psA = 1, psC = V.empty, psAb = 12, psBac = [nfTxt|sss|]}
    "prod_sort_1"

data PSortInner = PSortInner
  { psiB :: Word16,
    psiAa :: Bool
  }
  deriving (Show, Eq)

deriveProductGCbor
  (simpleProductOptions (toSnakeCase 3) [])
  ''PSortInner

data PSortNested = PSortNested
  { psoI :: PSortInner,
    psoAa :: Word32,
    psoCab :: Int64
  }
  deriving (Show, Eq)

deriveProductGCbor
  (simpleProductOptions (toSnakeCase 3) [])
  ''PSortNested

sortNested :: Spec
sortNested = describe "sort_nested" do
  mkTest
    "0"
    PSortNested
      { psoI = PSortInner {psiB = 10, psiAa = True},
        psoAa = 1000,
        psoCab = -6
      }
    "prod_sort_nested_0"
  mkTest
    "bound"
    PSortNested
      { psoI = PSortInner {psiB = 10, psiAa = True},
        psoCab = -255,
        psoAa = 1000
      }
    "prod_sort_nested_1"

{- Field omission test name: concat of
    R ++ number of required fields or,
    O ++ number of omissible fields -}

omitTest :: (Show v, Eq v, FromGCbor v, ToGCbor v) => [Word] -> v -> Spec
omitTest omit v =
  let abbr = toAbbr omit
   in mkTest abbr v ("prod_omit_" ++ abbr)
  where
    toAbbr [] = "full"
    toAbbr fs = foldMap (\f -> 'o' : show f) fs

data POmitR2O1R2 = POmitR2O1R2
  { po0F0 :: Word8,
    po0F1 :: Word8,
    po0F2 :: Maybe Word8,
    po0F3 :: Word8,
    po0F4 :: Word8
  }
  deriving (Show, Eq)

deriveProductGCbor
  (simpleProductOptions (toSnakeCase 3) ["po0F2"])
  ''POmitR2O1R2

omitR2O1R1 :: Spec
omitR2O1R1 = describe "r2o1r2" do
  let full =
        POmitR2O1R2
          { po0F0 = 0,
            po0F1 = 1,
            po0F2 = Just 2,
            po0F3 = 3,
            po0F4 = 4
          }
  omitTest [] full
  omitTest [2] full {po0F2 = Nothing}

data POmitR1O3R1 = POmitR1O3R1
  { po1F0 :: Word8,
    po1F1, po1F2, po1F3 :: Maybe Word8,
    po1F4 :: Word8
  }
  deriving (Show, Eq)

deriveProductGCbor
  (simpleProductOptions (toSnakeCase 3) ["po1F1", "po1F2", "po1F3"])
  ''POmitR1O3R1

omitR1O3R1 :: Spec
omitR1O3R1 = describe "r1o3r1" do
  let full =
        POmitR1O3R1
          { po1F0 = 0,
            po1F1 = Just 1,
            po1F2 = Just 2,
            po1F3 = Just 3,
            po1F4 = 4
          }
  omitTest [] full
  omitTest [1] full {po1F1 = Nothing}
  omitTest [2] full {po1F2 = Nothing}
  omitTest [3] full {po1F3 = Nothing}
  omitTest [1, 2] full {po1F1 = Nothing, po1F2 = Nothing}
  omitTest [2, 3] full {po1F2 = Nothing, po1F3 = Nothing}
  omitTest [1, 3] full {po1F1 = Nothing, po1F3 = Nothing}
  omitTest [1, 2, 3] full {po1F1 = Nothing, po1F2 = Nothing, po1F3 = Nothing}

data POmitO3R2 = POmitO3R2
  { po2F0, po2F1, po2F2 :: Maybe Word8,
    po2F3, po2F4 :: Word8
  }
  deriving (Show, Eq)

deriveProductGCbor
  (simpleProductOptions (toSnakeCase 3) ["po2F0", "po2F1", "po2F2"])
  ''POmitO3R2

omitO3R2 :: Spec
omitO3R2 = describe "o3r2" do
  let full =
        POmitO3R2
          { po2F0 = Just 0,
            po2F1 = Just 1,
            po2F2 = Just 2,
            po2F3 = 3,
            po2F4 = 4
          }
  omitTest [] full
  omitTest [0] full {po2F0 = Nothing}
  omitTest [1] full {po2F1 = Nothing}
  omitTest [2] full {po2F2 = Nothing}
  omitTest [0, 1] full {po2F0 = Nothing, po2F1 = Nothing}
  omitTest [1, 2] full {po2F1 = Nothing, po2F2 = Nothing}
  omitTest [0, 2] full {po2F0 = Nothing, po2F2 = Nothing}
  omitTest [0, 1, 2] full {po2F0 = Nothing, po2F1 = Nothing, po2F2 = Nothing}

data POmitR2O3 = POmitR2O3
  { po3F0, po3F1 :: Word8,
    po3F2, po3F3, po3F4 :: Maybe Word8
  }
  deriving (Show, Eq)

deriveProductGCbor
  (simpleProductOptions (toSnakeCase 3) ["po3F2", "po3F3", "po3F4"])
  ''POmitR2O3

omitR2O3 :: Spec
omitR2O3 = describe "r2o3" do
  let full =
        POmitR2O3
          { po3F0 = 0,
            po3F1 = 1,
            po3F2 = Just 2,
            po3F3 = Just 3,
            po3F4 = Just 4
          }
  omitTest [] full
  omitTest [2] full {po3F2 = Nothing}
  omitTest [3] full {po3F3 = Nothing}
  omitTest [4] full {po3F4 = Nothing}
  omitTest [2, 3] full {po3F2 = Nothing, po3F3 = Nothing}
  omitTest [3, 4] full {po3F3 = Nothing, po3F4 = Nothing}
  omitTest [2, 4] full {po3F2 = Nothing, po3F4 = Nothing}
  omitTest [2, 3, 4] full {po3F2 = Nothing, po3F3 = Nothing, po3F4 = Nothing}

spec :: Spec
spec = describe "product" do
  normal
  record
  pSort
  sortNested
  describe "omit" do
    omitR2O1R1
    omitR1O3R1
    omitO3R2
    omitR2O3