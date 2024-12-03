{-# LANGUAGE BlockArguments #-}

module Common (mkTest) where

import qualified Data.ByteString as BS
import System.FilePath
import Test.Hspec
import Webar.Codec.GCbor

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