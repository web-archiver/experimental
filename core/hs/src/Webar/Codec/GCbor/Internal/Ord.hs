module Webar.Codec.GCbor.Internal.Ord (GCborOrd (..)) where

import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Text as T
import qualified Data.UUID.Types as UUID
import Data.Word (Word16, Word32, Word64, Word8)
import qualified Webar.Text.Normalized as NF

class (Eq v) => GCborOrd v where
  -- | compare serialized cbor byte sequence
  compareGCbor :: v -> v -> Ordering

instance GCborOrd NF.NFText where
  compareGCbor nt1 nt2 =
    let t1 = NF.toText nt1
        t2 = NF.toText nt2
     in case compare (T.length t1) (T.length t2) of
          EQ -> compare t1 t2
          v -> v

instance GCborOrd Word8 where
  compareGCbor = compare

instance GCborOrd Word16 where
  compareGCbor = compare

instance GCborOrd Word32 where
  compareGCbor = compare

instance GCborOrd Word64 where
  compareGCbor = compare

compareInt :: (Ord a, Num a) => a -> a -> Ordering
compareInt l r = case (l >= 0, r >= 0) of
  (True, True) -> compare l r
  (True, False) -> LT
  (False, True) -> GT
  (False, False) -> compare r l

instance GCborOrd Int8 where
  compareGCbor = compareInt

instance GCborOrd Int16 where
  compareGCbor = compareInt

instance GCborOrd Int32 where
  compareGCbor = compareInt

instance GCborOrd Int64 where
  compareGCbor = compareInt

instance GCborOrd Bool where
  compareGCbor = compare

instance GCborOrd () where
  compareGCbor _ _ = EQ

instance GCborOrd UUID.UUID where
  compareGCbor = compare