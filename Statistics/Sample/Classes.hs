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
  , Monoid(..)
  , Calc(..)
  , Sample(..)
    -- * Hidden state
  , FoldM(..)
  , Fold(..)
  , extract
  , duplicate
  , extractM
  , duplicateM
  , estimator
    -- ** Dictionaries
  , NoDict(..)
  , MonoidDict(..)
  , MergeDict(..)
  , monoidDict
    -- * Derived combinators
  , accumElements
  , evalStatistics
  -- , mapReduceEst
  , mapReduce
  ) where

import Control.Arrow       ((***))
import Control.Applicative (Applicative(..))
import Control.Monad       (liftM)

import Data.Monoid (Monoid(..))
import Data.List   (foldl')

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
class Monoid m => FoldEstimator m a where
  -- | Add one element to sample
  addElement :: m -> a -> m

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

-- | Monadic fold.
data FoldM m a b = forall x. FoldM (x -> a -> m x) !x (x -> m b)

-- | Pure fold with additional dictionaries
data Fold d a b = forall x. Fold (x -> a -> x) !x (x -> b) (d x)

extract :: Fold d a b -> b
extract (Fold _ x out _) = out x

duplicate :: Fold d a b -> Fold d a (Fold d a b)
duplicate (Fold f x0 out d) = Fold f x0 (\x -> Fold f x out d) d

extractM :: FoldM m a b -> m b
extractM (FoldM _ x out) = out x

duplicateM :: Monad m => FoldM m a b -> FoldM m a (FoldM m a b)
duplicateM (FoldM f x0 out) = FoldM f x0 (\x -> return $ FoldM f x out)


instance Monad m => Functor (FoldM m a) where
  fmap f (FoldM step x out) = FoldM step x (liftM f . out)

instance Monad m => Applicative (FoldM m a) where
  pure x = FoldM (\_ _ -> return ()) () (\_ -> return x)
  FoldM stepA xA0 outA <*> FoldM stepB xB0 outB
    = FoldM step (xA0,xB0) out
    where
      step (xA,xB) a = do xA' <- stepA xA a
                          xB' <- stepB xB a
                          return (xA',xB')
      out (xA,xB) = do f <- outA xA
                       g <- outB xB
                       return $ f g

instance Functor (Fold d a) where
  fmap f (Fold step x out d) = Fold step x (f . out) d

instance MergeDict d => Applicative (Fold d a) where
  pure x = Fold (\_ _ -> ()) () (const x) unitDict
  Fold stepA xA0 outA dA <*> Fold stepB xB0 outB dB
    = Fold step (xA0,xB0) out (mergeDict dA dB)
    where
      step (xA,xB) a = let xA' = stepA xA a
                           xB' = stepB xB a
                       in (xA',xB')
      out (xA,xB) = outA xA $ outB xB

estimator :: (FoldEstimator m a) => Fold (MonoidDict NoDict) a m
{-# INLINE estimator #-}
estimator
  = Fold addElement mempty id (monoidDict NoDict)



----------------------------------------------------------------
-- Explicit dictionaries
----------------------------------------------------------------

-- | Explicit type class dictionaries
data NoDict x = NoDict

-- | Dictionary for monoids
data MonoidDict d x = MonoidDict x (x -> x -> x) (d x)

monoidDict :: Monoid x => d x -> MonoidDict d x
monoidDict = MonoidDict mempty mappend

-- | Type class for dictionaries which is necessary to define
--   'Applicative' instance for 'Fold'
class MergeDict d where
  unitDict   :: d ()
  mergeDict  :: d a -> d b -> d (a,b)

instance MergeDict NoDict where
  unitDict      = NoDict
  mergeDict _ _ = NoDict

instance MergeDict d => MergeDict (MonoidDict d) where
  unitDict = MonoidDict mempty mappend unitDict
  mergeDict (MonoidDict oA fA dA) (MonoidDict oB fB dB)
   = MonoidDict (oA,oB) (\(a,b) (c,d) -> (fA a c, fB b d)) (mergeDict dA dB)



----------------------------------------------------------------
-- Derived functions
----------------------------------------------------------------

-- | Add elements from sample to accumulator
accumElements :: (Sample s, FoldEstimator m (Elem s)) => m -> s -> m
accumElements = foldSample addElement
{-# INLINE accumElements #-}

-- | Evaluate statistics over sample.
evalStatistics :: (Sample a, FoldEstimator m (Elem a)) => a -> m
evalStatistics = foldSample addElement mempty
{-# INLINE evalStatistics #-}

mapReduce :: (FoldEstimator m a, Sample s)
          => (Elem s -> a) -> s -> m
mapReduce f
  = mapReduceSample mempty mappend f addElement
{-# INLINE mapReduce #-}


{-
mapReduceEst :: (Sample s)
             => Estimator a b -> (Elem s -> a) -> s -> b
mapReduceEst (Estimator fold _ out x0 merge) f s =
-- FIXME: I need somehow pass dictionary for estimator as well!
  out $ mapReduceSample x0 merge f fold s
-}



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
