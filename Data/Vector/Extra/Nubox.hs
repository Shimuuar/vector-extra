{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
module Data.Vector.Extra.Nubox where

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M

-- | Immutable unboxed vector.
newtype Vector a = Vector (IRepr (Representation a) a)

-- | Mutable unboxed vector
newtype MVector s a = MVector (MRepr (Representation a) s a)

type instance G.Mutable Vector = MVector



type family Representation a

data family IRepr tag :: * -> *
data family MRepr tag :: * -> * -> *

instance (M.MVector (MRepr (Representation a)) a) => M.MVector MVector a where
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
  basicLength (MVector v)                   = M.basicLength v
  basicUnsafeSlice i n (MVector v)          = MVector $ M.basicUnsafeSlice i n v
  basicOverlaps (MVector v1) (MVector v2)   = M.basicOverlaps v1 v2
  basicUnsafeNew n                          = MVector <$> M.basicUnsafeNew n
  basicInitialize (MVector v)               = M.basicInitialize v
  basicUnsafeReplicate n x                  = MVector <$> M.basicUnsafeReplicate n x
  basicUnsafeRead (MVector v) i             = M.basicUnsafeRead v i
  basicUnsafeWrite (MVector v) i x          = M.basicUnsafeWrite v i x
  basicClear (MVector v)                    = M.basicClear v
  basicSet (MVector v) x                    = M.basicSet v x
  basicUnsafeCopy (MVector v1) (MVector v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MVector v1) (MVector v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MVector v) n             = MVector <$> M.basicUnsafeGrow v n

instance ( M.MVector (MRepr (Representation a)) a
         , G.Vector  (IRepr (Representation a)) a
         , G.Mutable (IRepr (Representation a)) ~ MRepr (Representation a)
         ) => G.Vector Vector a where
  {-# INLINE basicUnsafeFreeze  #-}
  {-# INLINE basicUnsafeThaw    #-}
  {-# INLINE basicLength        #-}
  {-# INLINE basicUnsafeSlice   #-}
  {-# INLINE basicUnsafeIndexM  #-}
  {-# INLINE elemseq            #-}
  basicUnsafeFreeze (MVector v)           = Vector <$> G.basicUnsafeFreeze v
  basicUnsafeThaw (Vector v)              = MVector <$> G.basicUnsafeThaw v
  basicLength (Vector v)                  = G.basicLength v
  basicUnsafeSlice i n (Vector v)         = Vector $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (Vector v) i          = G.basicUnsafeIndexM v i
  basicUnsafeCopy (MVector mv) (Vector v) = G.basicUnsafeCopy mv v
  elemseq _ a                             = G.elemseq (undefined :: Vector a) a
