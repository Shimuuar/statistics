{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE BangPatterns          #-}
module Statistics.Sample.Classes (
    -- * Type classes for estimators
    FoldEstimator(..)
  , NullEstimator(..)
  , SemigoupEst(..)
  , Calc(..)
  , accumElements
  , evalStatistics
    -- ** X
  , E(..)
  , estimateWith
  , asEstimator
    -- ** Non-empty samples
  , NonEmptyEst(..)
  , InitEst(..)
    -- * Type class for data samples
  , Sample(..)
  ) where


import Data.List  (foldl')

import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed   as U
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Storable  as S
-- import qualified Data.Vector.Generic   as G



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
class FoldEstimator m a where
  -- | Add one element to sample
  addElement  :: m -> a -> m

-- | Estimators which are defined for empty samples.
class NullEstimator m where
  nullEstimator :: m

-- | Statistic accumulators which admit efficient join.
class SemigoupEst m where
  joinSample :: m -> m -> m

-- | Extract statistics from given estimator
class Calc m r where
  calc :: m -> r

-- | Type tag for estimator
data E m = E


-- | Add elements from sample to accumulator
accumElements :: (Sample a, FoldEstimator m (Elem a)) => m -> a -> m
accumElements = foldSample addElement
{-# INLINE accumElements #-}

-- | Evaluate statistics over sample.
evalStatistics :: (Sample a, FoldEstimator m (Elem a), NullEstimator m) => a -> m
evalStatistics = foldSample addElement nullEstimator
{-# INLINE evalStatistics #-}

-- | Estimate some statistics for sample
estimateWith :: (Sample a, FoldEstimator m (Elem a), NullEstimator m, Calc m r)
             => E m -- ^ Type parameter for selecting estimator
             -> a   -- ^ Data sample
             -> r   -- ^ Statistics to calculate
estimateWith est xs = calc $ evalStatistics xs `asEstimator` est
{-# INLINE estimateWith #-}

-- | Select type of estimator. Similar to 'asTypeOf'
asEstimator :: m -> E m -> m
asEstimator x _ = x



----------------------------------------------------------------
-- Estimator which doesn't work for nonempty samples
----------------------------------------------------------------

data InitEst a m
  = Init (a -> InitEst a m)
  | Est  m

class FoldEstimator m a => NonEmptyEst m a where
  nonemptyEst :: InitEst a m

instance FoldEstimator m a => FoldEstimator (InitEst a m) a where
  addElement (Init f) x = f x
  addElement (Est  m) x = Est $ addElement m x
  {-# INLINE addElement #-}

instance SemigoupEst m => SemigoupEst (InitEst a m) where
  joinSample (Est  m) (Est  n) = Est  $ joinSample m n
  joinSample (Init f)  m       = Init $ \x -> joinSample (f x) m
  joinSample m        (Init f) = Init $ \x -> joinSample (f x) m
  {-# INLINE joinSample #-}

instance Calc m r => Calc (InitEst a m) (Maybe r) where
  calc (Init _) = Nothing
  calc (Est  x) = Just $ calc x



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



----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

-- Estimators

instance (FoldEstimator a x, FoldEstimator b x) => FoldEstimator (a,b) x where
  addElement (!a,!b) x = (addElement a x, addElement b x)
  {-# INLINE addElement #-}

instance (NullEstimator a, NullEstimator b) => NullEstimator (a,b) where
  nullEstimator = (nullEstimator, nullEstimator)
  {-# INLINE nullEstimator #-}

instance (SemigoupEst a, SemigoupEst b) => SemigoupEst (a,b) where
  joinSample (!a1,!b1) (!a2,!b2) = (joinSample a1 a2, joinSample b1 b2)
  {-# INLINE joinSample #-}

instance (Calc m r, Calc m q) => Calc m (r,q) where
  calc m = (calc m, calc m)

instance (Calc m r, Calc m q, Calc m s) => Calc m (r,q,s) where
  calc m = (calc m, calc m, calc m)

instance (Calc m r, Calc m q, Calc m s, Calc m t) => Calc m (r,q,s,t) where
  calc m = (calc m, calc m, calc m, calc m)


-- Sample

instance Sample [a] where
  type Elem [a] = a
  foldSample              = foldl'
  {-# INLINE foldSample  #-}

instance U.Unbox a => Sample (U.Vector a) where
  type Elem (U.Vector a) = a
  foldSample             = U.foldl'
  {-# INLINE foldSample  #-}

instance S.Storable a => Sample (S.Vector a) where
  type Elem (S.Vector a) = a
  foldSample             = S.foldl'
  {-# INLINE foldSample  #-}

instance P.Prim a => Sample (P.Vector a) where
  type Elem (P.Vector a) = a
  foldSample             = P.foldl'
  {-# INLINE foldSample  #-}

instance Sample (V.Vector a) where
  type Elem (V.Vector a) = a
  foldSample             = V.foldl'
  {-# INLINE foldSample  #-}
