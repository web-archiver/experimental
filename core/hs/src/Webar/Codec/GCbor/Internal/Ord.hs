module Webar.Codec.GCbor.Internal.Ord
  ( GCborOrd (..),
    compareText,
  )
where

import Data.Int (Int16, Int32, Int64, Int8)
import qualified Data.Text as T
import qualified Data.Text.Array as TA
import qualified Data.Text.Internal as TI
import qualified Data.UUID.Types as UUID
import Data.Word (Word16, Word32, Word64, Word8)
import qualified Webar.Text.Normalized as NF

class (Eq v) => GCborOrd v where
  -- | compare serialized cbor byte sequence
  compareGCbor :: v -> v -> Ordering

compareText :: T.Text -> T.Text -> Ordering
compareText (TI.Text arr0 off0 len0) (TI.Text arr1 off1 len1) =
  case compare len0 len1 of
    EQ -> TA.compare arr0 off0 arr1 off1 len0
    v -> v

instance GCborOrd NF.NFText where
  compareGCbor nt1 nt2 = compareText (NF.toText nt1) (NF.toText nt2)

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