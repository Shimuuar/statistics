{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE BangPatterns          #-}
-- |

-- Calculation of many statistics be expressed as fold over data
-- sample (for example: number of element, mean, any moment,
-- minimum\/maximum etc.). So it's useful to expose these folds.
-- Moreover many fold accumulators allow efficient join which
-- corresponds to union of data samples.
--
-- However we cannot expose accumulator type for some estimators. For
-- example robust variance estimator or n-th central moment depend on
-- parameter mean for former and mean and number of moment for latter.
--
-- WRITEME: Double traversals (robust variance).
module Statistics.Sample.Classes (
    -- * Type classes for estimators
    FoldEstimator(..)
  , NullEstimator(..)
  , MonoidEst(..)
  , Calc(..)
  , Sample(..)
    -- * Hidden state
  , Estimator(..)
  , estimator
    -- * Derived combinators
  , accumElements
  , evalStatistics
  , mapReduceEst
  , mapReduce
  ) where

import Control.Arrow       ((***))
import Control.Applicative (Applicative(..))

import Data.List  (foldl')

import qualified Data.Foldable         as F
import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed   as U
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Storable  as S
import qualified Data.Vector.Generic   as G



----------------------------------------------------------------
-- Estimators
----------------------------------------------------------------

-- | Type class for statistics which could be expresed by fold.
class FoldEstimator m a where
  -- | Add one element to sample
  addElement  :: m -> a -> m

-- | Estimators which are defined for empty samples.
class NullEstimator m where
  nullEstimator :: m

-- | Statistic accumulators which admit efficient join.
class NullEstimator m => MonoidEst m where
  mergeSamples  :: m -> m -> m

-- | Extract statistics from given estimator. Type of statistics is
--   encoded in return type.
class Calc m r where
  calc :: m -> r


-- | Type class for samples. Since statistics doesn't depend on
--   element order sample could be folded in any order.
--
--   It's stripped down version of 'Foldable' but since vectors
--   couldn't be instances of foldable this type class is required.
class Sample s where
  -- | Element type of a sample/
  type Elem s :: *
  -- | Strict fold over sample.
  foldSample  :: (m -> Elem s -> m) -> m -> s -> m
  -- | We have to pass dictionary explicitly
  mapReduceSample ::  m
                  -> (m -> m -> m)
                  -> (Elem s -> a)
                  -> (m -> a -> m)
                  -> s
                  -> m
  mapReduceSample x _ toA fold
    = foldSample (\m -> fold m . toA) x



----------------------------------------------------------------
-- Hidden state
----------------------------------------------------------------

-- | For estimators
data Estimator a b = forall x. Estimator
  { estFold  :: x -> a -> x     -- ^ Fold function
  , estState :: !x              -- ^ Current content of accumulator
  , estOut   :: x -> b          -- ^ Convert state to the output
  , estNull  :: !x              -- ^ Estimator for empty sample
  , estMerge :: x -> x -> x     -- ^ Function to merge two different estimators
  }

instance FoldEstimator (Estimator a b) a where
  addElement (Estimator fold x out x0 merge) a
    = Estimator fold (fold x a) out x0 merge

instance Functor (Estimator a) where
  fmap f (Estimator fold x out none merge)
    = Estimator fold x (f . out) none merge

instance Applicative (Estimator a) where
  pure x = Estimator const x id x const
  Estimator foldA xA outA noneA mergeA <*> Estimator foldB xB outB noneB mergeB
    = Estimator (\(mA,mB) a -> (foldA mA a, foldB mB a))
                (xA,xB)
                (\(mA,mB) -> outA mA $ outB mB)
                (noneA,noneB)
                (\(mA,mB) (nA,nB) -> (mergeA mA nA, mergeB mB nB))

estimator :: (FoldEstimator m a, MonoidEst m) => Estimator a m
{-# INLINE estimator #-}
estimator
  = Estimator addElement nullEstimator id nullEstimator mergeSamples


----------------------------------------------------------------
-- Derived functions
----------------------------------------------------------------

-- | Add elements from sample to accumulator
accumElements :: (Sample s, FoldEstimator m (Elem s)) => m -> s -> m
accumElements = foldSample addElement
{-# INLINE accumElements #-}

-- | Evaluate statistics over sample.
evalStatistics :: (Sample a, FoldEstimator m (Elem a), NullEstimator m) => a -> m
evalStatistics = foldSample addElement nullEstimator
{-# INLINE evalStatistics #-}

mapReduce :: (MonoidEst m, FoldEstimator m a, Sample s)
          => (Elem s -> a) -> s -> m
mapReduce f
  = mapReduceSample nullEstimator mergeSamples f addElement
{-# INLINE mapReduce #-}



mapReduceEst :: (Sample s)
             => Estimator a b -> (Elem s -> a) -> s -> b
mapReduceEst (Estimator fold _ out x0 merge) f s =
-- FIXME: I need somehow pass dictionary for estimator as well!
  out $ mapReduceSample x0 merge f fold s




----------------------------------------------------------------
-- Sample
----------------------------------------------------------------


{-
-- | Netype wrapper for 'Foldable' instances
newtype FoldableSample f a = FoldableSample { getFoldableSample :: f a }

instance F.Foldable f => Sample (FoldableSample f a) where
  type Elem (FoldableSample f a) = a
  foldSample f a (FoldableSample xs) = F.foldl' f a xs


-- | Convert arbitrary sample to vector
sampleToVector :: (Sample s, G.Vector v (Elem s)) => s -> v (Elem s)
sampleToVector = foldSample G.snoc G.empty

-- | Converts arbitrary sample to list. Elements will appear in
--   reverse order.
sampleToList :: (Sample s) => s -> [Elem s]
sampleToList = foldSample (flip (:)) []
-}


----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance (FoldEstimator a x, FoldEstimator b x) => FoldEstimator (a,b) x where
  addElement (!a,!b) x = (addElement a x, addElement b x)
  {-# INLINE addElement #-}

instance (NullEstimator a, NullEstimator b) => NullEstimator (a,b) where
  nullEstimator = (nullEstimator, nullEstimator)
  {-# INLINE nullEstimator #-}

instance (MonoidEst a, MonoidEst b) => MonoidEst (a,b) where
  mergeSamples (!a1,!b1) (!a2,!b2) = (mergeSamples a1 a2, mergeSamples b1 b2)
  {-# INLINE mergeSamples #-}

instance (Calc m r, Calc m q) => Calc m (r,q) where
  calc m = (calc m, calc m)

instance (Calc m r, Calc m q, Calc m s) => Calc m (r,q,s) where
  calc m = (calc m, calc m, calc m)

instance (Calc m r, Calc m q, Calc m s, Calc m t) => Calc m (r,q,s,t) where
  calc m = (calc m, calc m, calc m, calc m)



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
