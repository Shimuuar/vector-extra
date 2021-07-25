{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import Control.Monad.Fix
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
  basicLength (MReversed v) = M.basicLength v -- COERCE
  {-# INLINE basicLength #-}
  basicOverlaps (MReversed v) (MReversed u) = M.basicOverlaps v u -- COERCE
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MReversed <$> M.basicUnsafeNew n -- COERCE
  {-# INLINE basicUnsafeNew #-}
  basicInitialize (MReversed v) = M.basicInitialize v -- COERCE
  {-# INLINE basicInitialize #-}
  basicUnsafeReplicate i a = MReversed <$> M.basicUnsafeReplicate i a -- COERCE
  {-# INLINE basicUnsafeReplicate #-}
  basicClear (MReversed v) = M.basicClear v  -- COERCE
  {-# INLINE basicClear #-}
  -- FIXME: Is this correct?
  basicUnsafeCopy (MReversed v) (MReversed u) = M.basicUnsafeCopy v u
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MReversed v) (MReversed u) = M.basicUnsafeMove v u
  {-# INLINE basicUnsafeMove #-}
  -- FIXME: wrong direction
  basicUnsafeGrow (MReversed v) i = MReversed <$> M.basicUnsafeGrow v i -- COERCE
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
  basicUnsafeFreeze (MReversed v) = Reversed <$> G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (Reversed v) = MReversed <$> G.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (Reversed v) = G.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeCopy (MReversed v) (Reversed u) = G.basicUnsafeCopy v u
  {-# INLINE basicUnsafeCopy #-}
  -- Index manipulations
  basicUnsafeSlice i k (Reversed v) = Reversed $ G.basicUnsafeSlice (n - i - k) k v
    where n = G.basicLength v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (Reversed v) i = G.basicUnsafeIndexM v (G.basicLength v - i - 1)
  {-# INLINE basicUnsafeIndexM #-}

reversing :: (Reversed v a -> Reversed v b) -> v a -> v b
reversing = coerce
