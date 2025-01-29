{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
module Statistics.Vector
  ( VecPair(..)
  , MVecPair(..)
  , pattern UnzipPair
  ) where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as M


pattern UnzipPair :: (G.Vector va a, G.Vector vb b) => va a -> vb b -> VecPair va vb (a,b)
pattern UnzipPair va vb <- VecPair va vb where
  UnzipPair va vb = let n = G.length va `min` G.length vb in VecPair (G.take n va) (G.take n vb)
{-# COMPLETE UnzipPair #-}

data VecPair va vb x where
  VecPair :: !(va a) -> !(vb b) -> VecPair va vb (a,b)

data MVecPair va vb s x where
  MVecPair :: !(G.Mutable va s a) -> !(G.Mutable vb s b) -> MVecPair va vb s (a,b)


type instance G.Mutable (VecPair va vb) = MVecPair va vb

instance (G.Vector va a, G.Vector vb b) => G.Vector (VecPair va vb) (a,b) where
  basicUnsafeFreeze (MVecPair mva mvb) = do
    va <- G.basicUnsafeFreeze mva
    vb <- G.basicUnsafeFreeze mvb
    pure $! VecPair va vb
  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (VecPair va vb) = do
    mva <- G.basicUnsafeThaw va
    mvb <- G.basicUnsafeThaw vb
    pure $! MVecPair mva mvb
  {-# INLINE basicLength #-}
  basicLength (VecPair va _) = G.basicLength va
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i n (VecPair va vb)
    = VecPair (G.basicUnsafeSlice i n va) (G.basicUnsafeSlice i n vb)
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (VecPair va vb) i = do
    a <- G.basicUnsafeIndexM va i
    b <- G.basicUnsafeIndexM vb i
    pure (a,b)
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVecPair mva mvb) (VecPair va vb) = do
    G.basicUnsafeCopy mva va
    G.basicUnsafeCopy mvb vb



instance (G.Vector va a, G.Vector vb b) => M.MVector (MVecPair va vb) (a,b) where
  {-# INLINE basicLength #-}
  basicLength (MVecPair va _) = M.basicLength va
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice i n (MVecPair va vb)
    = MVecPair (M.basicUnsafeSlice i n va) (M.basicUnsafeSlice i n vb)
  {-# INLINE basicOverlaps #-}
  basicOverlaps (MVecPair va1 vb1) (MVecPair va2 vb2) =
    M.basicOverlaps va1 va2 || M.basicOverlaps vb1 vb2
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew n = do
    va <- M.basicUnsafeNew n
    vb <- M.basicUnsafeNew n
    pure $ MVecPair va vb
  {-# INLINE basicInitialize #-}
  basicInitialize (MVecPair va vb) =
    M.basicInitialize va >> M.basicInitialize vb
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeReplicate n (a,b) = do
    va <- M.basicUnsafeReplicate n a
    vb <- M.basicUnsafeReplicate n b
    pure $ MVecPair va vb
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MVecPair va vb) i = do
    a <- M.basicUnsafeRead va i
    b <- M.basicUnsafeRead vb i
    pure (a,b)
  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MVecPair va vb) i (a,b) = do
    M.basicUnsafeWrite va i a
    M.basicUnsafeWrite vb i b
  {-# INLINE basicClear #-}
  basicClear (MVecPair va vb) = M.basicClear va >> M.basicClear vb
  {-# INLINE basicSet #-}
  basicSet (MVecPair va vb) (a,b) = M.basicSet va a >> M.basicSet vb b
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVecPair t_a t_b) (MVecPair s_a s_b) = do
    M.basicUnsafeCopy t_a s_a
    M.basicUnsafeCopy t_b s_b
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (MVecPair t_a t_b) (MVecPair s_a s_b) = do
    M.basicUnsafeMove t_a s_a
    M.basicUnsafeMove t_b s_b
  {-# INLINE basicUnsafeGrow #-}
  basicUnsafeGrow (MVecPair va vb) i = do
    va' <- M.basicUnsafeGrow va i
    vb' <- M.basicUnsafeGrow vb i
    pure $! MVecPair va' vb'



