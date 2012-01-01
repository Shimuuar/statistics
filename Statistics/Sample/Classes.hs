{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE EmptyDataDecls        #-}
module Statistics.Sample.Classes (
    -- * Type class for data samples
    Sample(..)
  , accumElements  
  , evalStatistics
  , evalStatistics1
    -- * Type class for estimators
  , FoldEstimator(..)
  , addElement
  , SingletonEst(..)
  , NullEstimator(..)
  , SemigoupEst(..)
    -- ** Multiple element types
  , Accept(..)
  , EstimatorType
  , estimatorType
  ) where


import Data.List  (foldl')
import Data.Maybe (fromJust)

import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed   as U
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Storable  as S
import qualified Data.Vector.Generic   as G

----------------------------------------------------------------
-- Sample
----------------------------------------------------------------

-- | Type class for samples. Since statistics doesn't depend on
--   element order sample could be folded in any order.
--
--   It's stripped down version of 'Foldable' but since vectors
--   couldn't be instances of foldable this type class is required.
class Sample a where
  -- | Element type of a sample/
  type Elem a :: *
  -- | Strict fold over sample.
  foldSample  :: (acc -> Elem a -> acc) -> acc -> a -> acc
  -- | Variant of fold. Returns 'Nothing' for empty samples.
  foldSample1 :: (Elem a -> acc)        -- ^ Transform first element into accumulator
              -> (acc -> Elem a -> acc) -- ^ Accumulation function
              -> a                      -- ^ Sample
              -> Maybe acc
                           

-- | Add elements from sample to accumulator
accumElements :: (Sample a, FoldEstimator m, Accept m (Elem a)) => m -> a -> m
accumElements = foldSample addElement
{-# INLINE accumElements #-}

-- | Evaluate statistics over sample.
evalStatistics :: (Sample a, NullEstimator m, Accept m (Elem a)) => a -> m
evalStatistics = foldSample addElement nullEstimator
{-# INLINE evalStatistics #-}


evalStatistics1 :: (Sample a, SingletonEst m, Accept m (Elem a)) => a -> Maybe m
evalStatistics1 xs = res
  where
    res = foldSample1 (singletonStat . transformElem m) addElement xs
    m   = estimatorType (fromJust res)
{-# INLINE evalStatistics1 #-}


-- Instances

instance Sample [a] where
  type Elem [a] = a
  foldSample              = foldl'
  foldSample1 _  _ []     = Nothing
  foldSample1 tr f (x:xs) = Just $ foldl' f (tr x) xs
  {-# INLINE foldSample  #-}
  {-# INLINE foldSample1 #-}

instance U.Unbox a => Sample (U.Vector a) where
  type Elem (U.Vector a) = a
  foldSample             = U.foldl'
  foldSample1 tr f vec   = vectorFoldSample1 tr f vec
  {-# INLINE foldSample  #-}
  {-# INLINE foldSample1 #-}

instance S.Storable a => Sample (S.Vector a) where
  type Elem (S.Vector a) = a
  foldSample             = S.foldl'
  foldSample1 tr f vec   = vectorFoldSample1 tr f vec
  {-# INLINE foldSample  #-}
  {-# INLINE foldSample1 #-}

instance P.Prim a => Sample (P.Vector a) where
  type Elem (P.Vector a) = a
  foldSample             = P.foldl'
  foldSample1 tr f vec   = vectorFoldSample1 tr f vec
  {-# INLINE foldSample  #-}
  {-# INLINE foldSample1 #-}

instance Sample (V.Vector a) where
  type Elem (V.Vector a) = a
  foldSample             = V.foldl'
  foldSample1 tr f vec   = vectorFoldSample1 tr f vec
  {-# INLINE foldSample  #-}
  {-# INLINE foldSample1 #-}

vectorFoldSample1 :: (G.Vector v a) => (a -> m) -> (m -> a -> m) -> v a -> Maybe m
vectorFoldSample1 tr f vec
  | G.null vec = Nothing
  | otherwise  = Just $ G.foldl' f (tr $ G.head vec) (G.tail vec)
{-# INLINE vectorFoldSample1 #-}


                           
----------------------------------------------------------------
-- Estimators
----------------------------------------------------------------

-- Instances must obey following laws:
--
-- > joinSample x y = joinSample y x  -- commutativity
-- > joinSample nullStat x = x        -- left identity
-- > joinSample x nullStat = x        -- right identity
-- > 
-- > addStdElement m x = joinSample m (singletonStat x)

-- | Type class for statistics which could be expresed by fold.
class FoldEstimator m where
  -- | Standard elemnt of sample
  type StandardElem m :: *
  -- | Add one element to sample
  addStdElement  :: m -> StandardElem m -> m


addElement :: (FoldEstimator m, Accept m a) => m -> a -> m
addElement m = addStdElement m . transformElem (estimatorType m)
{-# INLINE addElement #-}


-- | Type class for estimators which are defined for single element
--   samples. It's nessesary to distinguish this type class from
--   'NullEstimator' since some estimators are defined for single
--   element samples but not for empty samples. E.g. minimum or
--   maximum value.
class FoldEstimator m => SingletonEst m where
  singletonStat :: StandardElem m -> m

-- | Estimators which are defined for empty samples.
class SingletonEst m => NullEstimator m where
  nullEstimator :: m

-- | Statistic accumulators which admit efficient join.
class FoldEstimator m => SemigoupEst m where
  joinSample :: m -> m -> m


----------------------------------------
  
data EstimatorType m

estimatorType :: m -> EstimatorType m
estimatorType _ = undefined
{-# INLINE estimatorType #-}

class FoldEstimator m => Accept m a where
  transformElem :: EstimatorType m -> a -> StandardElem m
