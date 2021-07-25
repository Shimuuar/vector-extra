{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Add different representations for unboxed vectors.
module Data.Vector.Extra.Unbox where

import Data.Coerce
import qualified Data.Vector.Primitive       as P
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M

----------------------------------------------------------------
-- Primitive representation
----------------------------------------------------------------

-- | Representation for unboxed vectors as primitive vectors. Copied
--   from vector in order to play with it
newtype UnboxViaPrim a = UnboxViaPrim a

newtype instance U.MVector s (UnboxViaPrim a) = MV_UnboxViaPrim (P.MVector s a)
newtype instance U.Vector    (UnboxViaPrim a) = V_UnboxViaPrim (P.Vector a)

instance P.Prim a => M.MVector U.MVector (UnboxViaPrim a) where
  basicLength (MV_UnboxViaPrim v) = M.basicLength v
  basicUnsafeSlice i n (MV_UnboxViaPrim v) = MV_UnboxViaPrim $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_UnboxViaPrim v1) (MV_UnboxViaPrim v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_UnboxViaPrim <$> M.basicUnsafeNew n
  basicInitialize (MV_UnboxViaPrim v) = M.basicInitialize v
  basicUnsafeReplicate n (UnboxViaPrim x) = MV_UnboxViaPrim <$> M.basicUnsafeReplicate n x
  basicUnsafeRead (MV_UnboxViaPrim v) i = UnboxViaPrim <$> M.basicUnsafeRead v i
  basicUnsafeWrite (MV_UnboxViaPrim v) i (UnboxViaPrim x) = M.basicUnsafeWrite v i x
  basicClear (MV_UnboxViaPrim v) = M.basicClear v
  basicSet (MV_UnboxViaPrim v) (UnboxViaPrim x) = M.basicSet v x
  basicUnsafeCopy (MV_UnboxViaPrim v1) (MV_UnboxViaPrim v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_UnboxViaPrim v1) (MV_UnboxViaPrim v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_UnboxViaPrim v) n = MV_UnboxViaPrim <$> M.basicUnsafeGrow v n
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}

instance P.Prim a => G.Vector U.Vector (UnboxViaPrim a) where
  basicUnsafeFreeze (MV_UnboxViaPrim v) = V_UnboxViaPrim <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (V_UnboxViaPrim v) = MV_UnboxViaPrim <$> G.basicUnsafeThaw v
  basicLength (V_UnboxViaPrim v) = G.basicLength v
  basicUnsafeSlice i n (V_UnboxViaPrim v) = V_UnboxViaPrim $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_UnboxViaPrim v) i = UnboxViaPrim <$> G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_UnboxViaPrim mv) (V_UnboxViaPrim v) = G.basicUnsafeCopy mv v
  elemseq _ = seq
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}

