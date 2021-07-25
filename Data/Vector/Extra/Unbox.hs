{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Add different representations for unboxed vectors.
module Data.Vector.Extra.Unbox where

import Data.Coerce
import qualified Data.Vector                   as V
import qualified Data.Vector.Primitive         as VP
import qualified Data.Vector.Primitive.Mutable as VPM
import qualified Data.Vector.Unboxed           as U
import qualified Data.Vector.Generic           as G
import qualified Data.Vector.Generic.Mutable   as M

----------------------------------------------------------------
-- Primitive representation
----------------------------------------------------------------

-- | Representation for unboxed vectors as primitive vectors. Copied
--   from vector in order to play with it
newtype UnboxViaPrim a = UnboxViaPrim a

newtype instance U.MVector s (UnboxViaPrim a) = MV_UnboxViaPrim (VP.MVector s a)
newtype instance U.Vector    (UnboxViaPrim a) = V_UnboxViaPrim (VP.Vector a)

instance VP.Prim a => M.MVector U.MVector (UnboxViaPrim a) where
  basicLength          = coerce $ M.basicLength          @VPM.MVector @a
  basicUnsafeSlice     = coerce $ M.basicUnsafeSlice     @VPM.MVector @a
  basicOverlaps        = coerce $ M.basicOverlaps        @VPM.MVector @a
  basicUnsafeNew       = coerce $ M.basicUnsafeNew       @VPM.MVector @a
  basicInitialize      = coerce $ M.basicInitialize      @VPM.MVector @a
  basicUnsafeReplicate = coerce $ M.basicUnsafeReplicate @VPM.MVector @a
  basicUnsafeRead      = coerce $ M.basicUnsafeRead      @VPM.MVector @a
  basicUnsafeWrite     = coerce $ M.basicUnsafeWrite     @VPM.MVector @a
  basicClear           = coerce $ M.basicClear           @VPM.MVector @a
  basicSet             = coerce $ M.basicSet             @VPM.MVector @a
  basicUnsafeCopy      = coerce $ M.basicUnsafeCopy      @VPM.MVector @a
  basicUnsafeMove      = coerce $ M.basicUnsafeMove      @VPM.MVector @a
  basicUnsafeGrow      = coerce $ M.basicUnsafeGrow      @VPM.MVector @a
  {-# INLINE basicLength          #-}
  {-# INLINE basicUnsafeSlice     #-}
  {-# INLINE basicOverlaps        #-}
  {-# INLINE basicUnsafeNew       #-}
  {-# INLINE basicInitialize      #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead      #-}
  {-# INLINE basicUnsafeWrite     #-}
  {-# INLINE basicClear           #-}
  {-# INLINE basicSet             #-}
  {-# INLINE basicUnsafeCopy      #-}
  {-# INLINE basicUnsafeGrow      #-}

instance VP.Prim a => G.Vector U.Vector (UnboxViaPrim a) where
  basicUnsafeFreeze = coerce $ G.basicUnsafeFreeze @VP.Vector @a
  basicUnsafeThaw   = coerce $ G.basicUnsafeThaw   @VP.Vector @a
  basicLength       = coerce $ G.basicLength       @VP.Vector @a
  basicUnsafeSlice  = coerce $ G.basicUnsafeSlice  @VP.Vector @a
  basicUnsafeIndexM = coerce $ G.basicUnsafeIndexM @VP.Vector @a
  basicUnsafeCopy   = coerce $ G.basicUnsafeCopy   @VP.Vector @a
  elemseq _ = seq
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw   #-}
  {-# INLINE basicLength       #-}
  {-# INLINE basicUnsafeSlice  #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq           #-}

