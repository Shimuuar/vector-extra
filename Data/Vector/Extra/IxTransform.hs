{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- |
module Data.Vector.Extra.IxTransform
  ( -- * Reversed vectors
    Reversed(..)
  , MReversed(..)
  , reversing
  ) where

import Control.Monad.ST
import Data.Coerce
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M

----------------------------------------------------------------
-- Reversed vector
----------------------------------------------------------------

-- | Newtype wrapper for vectors that looks on vector below in
--   reverse.
newtype Reversed v a = Reversed (v a)
  -- deriving newtype (Functor,Applicative,Monad,MonadFail,MonadFix)
  -- deriving stock   (Foldable,Traversable)

-- | Mutable reversed vector
newtype MReversed v s a = MReversed (v s a)

type instance G.Mutable (Reversed v) = MReversed (G.Mutable v)

instance (Show a, G.Vector v a) => Show (Reversed v a) where
  showsPrec = G.showsPrec
instance (Eq a, G.Vector v a) => Eq (Reversed v a) where
  (==) = G.eq
  {-# INLINE (==) #-}
instance (Ord a, G.Vector v a) => Ord (Reversed v a) where
  compare = G.cmp
  {-# INLINE compare #-}


instance M.MVector v a => M.MVector (MReversed v) a where
  basicLength :: forall s. MReversed v s a -> Int
  basicLength = coerce $ M.basicLength @v @a @s
  {-# INLINE basicLength #-}
  basicOverlaps :: forall s. MReversed v s a -> MReversed v s a -> Bool
  basicOverlaps = coerce $ M.basicOverlaps @v @a @s
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew       = coerce $ M.basicUnsafeNew       @v @a
  basicInitialize      = coerce $ M.basicInitialize      @v @a
  basicUnsafeReplicate = coerce $ M.basicUnsafeReplicate @v @a
  basicClear           = coerce $ M.basicClear           @v @a
  basicUnsafeCopy      = coerce $ M.basicUnsafeCopy      @v @a
  basicUnsafeMove      = coerce $ M.basicUnsafeMove      @v @a
  {-# INLINE basicUnsafeNew       #-}
  {-# INLINE basicInitialize      #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicClear           #-}
  {-# INLINE basicUnsafeCopy      #-}
  {-# INLINE basicUnsafeMove      #-}
  basicUnsafeGrow :: forall s. MReversed v s a -> Int -> ST s (MReversed v s a)
  basicUnsafeGrow = coerce $ M.unsafeGrowFront @(ST s) @v @a
  {-# INLINE basicUnsafeGrow #-}
  -- Index manipulations
  basicUnsafeRead (MReversed v) i = M.basicUnsafeRead v (M.basicLength v - i - 1)
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MReversed v) i = M.basicUnsafeWrite v (M.basicLength v - i - 1)
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeSlice i k (MReversed v) = MReversed (M.basicUnsafeSlice (n - i - k) k v)
    where n = M.basicLength v
  {-# INLINE basicUnsafeSlice #-}

instance G.Vector v a => G.Vector (Reversed v) a where
  basicLength       = coerce $ G.basicLength       @v @a
  basicUnsafeCopy   = coerce $ G.basicUnsafeCopy   @v @a
  basicUnsafeThaw   = coerce $ G.basicUnsafeThaw   @v @a
  basicUnsafeFreeze = coerce $ G.basicUnsafeFreeze @v @a
  {-# INLINE basicLength       #-}
  {-# INLINE basicUnsafeCopy   #-}
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw   #-}
  -- Index manipulations
  basicUnsafeSlice i k (Reversed v)
    = Reversed $ G.basicUnsafeSlice (n - i - k) k v
    where n = G.basicLength v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (Reversed v) i
    = G.basicUnsafeIndexM v (G.basicLength v - i - 1)
  {-# INLINE basicUnsafeIndexM #-}

reversing :: (Reversed v a -> Reversed v b) -> v a -> v b
reversing = coerce
