{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
-- |
-- Add different representations for unboxed vectors.
module Data.Vector.Extra.Unbox where

import Data.Coerce
import qualified Data.Vector                   as V
import qualified Data.Vector.Primitive         as VP
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

instance VP.Prim a =>U.Unbox (UnboxViaPrim a)

instance VP.Prim a => M.MVector U.MVector (UnboxViaPrim a) where
  basicLength          = coerce $ M.basicLength          @VP.MVector @a
  basicUnsafeSlice     = coerce $ M.basicUnsafeSlice     @VP.MVector @a
  basicOverlaps        = coerce $ M.basicOverlaps        @VP.MVector @a
  basicUnsafeNew       = coerce $ M.basicUnsafeNew       @VP.MVector @a
  basicInitialize      = coerce $ M.basicInitialize      @VP.MVector @a
  basicUnsafeReplicate = coerce $ M.basicUnsafeReplicate @VP.MVector @a
  basicUnsafeRead      = coerce $ M.basicUnsafeRead      @VP.MVector @a
  basicUnsafeWrite     = coerce $ M.basicUnsafeWrite     @VP.MVector @a
  basicClear           = coerce $ M.basicClear           @VP.MVector @a
  basicSet             = coerce $ M.basicSet             @VP.MVector @a
  basicUnsafeCopy      = coerce $ M.basicUnsafeCopy      @VP.MVector @a
  basicUnsafeMove      = coerce $ M.basicUnsafeMove      @VP.MVector @a
  basicUnsafeGrow      = coerce $ M.basicUnsafeGrow      @VP.MVector @a
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


----------------------------------------------------------------
-- Boxed representation
----------------------------------------------------------------

-- | Representation for unboxed vectors as boxed vectors. Not really
--   unboxed but allows to partially unbox data type. For example two
--   of three fields could be unboxed
--
-- > data Foo = Foo Text Int Int
newtype UnboxViaBoxed a = UnboxViaBoxed a

newtype instance U.MVector s (UnboxViaBoxed a) = MV_UnboxViaBoxed (V.MVector s a)
newtype instance U.Vector    (UnboxViaBoxed a) = V_UnboxViaBoxed  (V.Vector a)

instance U.Unbox (UnboxViaBoxed a)

instance M.MVector U.MVector (UnboxViaBoxed a) where
  basicLength          = coerce $ M.basicLength          @V.MVector @a
  basicUnsafeSlice     = coerce $ M.basicUnsafeSlice     @V.MVector @a
  basicOverlaps        = coerce $ M.basicOverlaps        @V.MVector @a
  basicUnsafeNew       = coerce $ M.basicUnsafeNew       @V.MVector @a
  basicInitialize      = coerce $ M.basicInitialize      @V.MVector @a
  basicUnsafeReplicate = coerce $ M.basicUnsafeReplicate @V.MVector @a
  basicUnsafeRead      = coerce $ M.basicUnsafeRead      @V.MVector @a
  basicUnsafeWrite     = coerce $ M.basicUnsafeWrite     @V.MVector @a
  basicClear           = coerce $ M.basicClear           @V.MVector @a
  basicSet             = coerce $ M.basicSet             @V.MVector @a
  basicUnsafeCopy      = coerce $ M.basicUnsafeCopy      @V.MVector @a
  basicUnsafeMove      = coerce $ M.basicUnsafeMove      @V.MVector @a
  basicUnsafeGrow      = coerce $ M.basicUnsafeGrow      @V.MVector @a
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


instance G.Vector U.Vector (UnboxViaBoxed a) where
  basicUnsafeFreeze = coerce $ G.basicUnsafeFreeze @V.Vector @a
  basicUnsafeThaw   = coerce $ G.basicUnsafeThaw   @V.Vector @a
  basicLength       = coerce $ G.basicLength       @V.Vector @a
  basicUnsafeSlice  = coerce $ G.basicUnsafeSlice  @V.Vector @a
  basicUnsafeIndexM = coerce $ G.basicUnsafeIndexM @V.Vector @a
  basicUnsafeCopy   = coerce $ G.basicUnsafeCopy   @V.Vector @a
  elemseq _ = seq
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw   #-}
  {-# INLINE basicLength       #-}
  {-# INLINE basicUnsafeSlice  #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq           #-}
