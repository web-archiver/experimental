module Webar.Codec.GCbor.Internal.Ord where

import qualified Data.Text as T
import qualified Webar.Text.Normalized as NF

class GCborOrd v where
  -- | compare serialized cbor byte sequence
  compareGCbor :: v -> v -> Ordering

instance GCborOrd NF.NFText where
  compareGCbor nt1 nt2 =
    let t1 = NF.toText nt1
        t2 = NF.toText nt2
     in case compare (T.length t1) (T.length t2) of
          EQ -> compare t1 t2
          v -> v