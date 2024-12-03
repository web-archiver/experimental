{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Data.Maybe (fromJust)
import qualified Data.Text as T
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Webar.Codec.GCbor
import qualified Webar.Text.Normalized as NF

main :: IO ()
main = hspec do
  prop "NFText" \(ASCIIString s1, ASCIIString s2) -> do
    let t1 = fromJust (NF.fromAscii (T.pack s1))
        t2 = fromJust (NF.fromAscii (T.pack s2))
    compareGCbor t1 t2
      `shouldBe` compare (encodeStrictBs t1) (encodeStrictBs t2)