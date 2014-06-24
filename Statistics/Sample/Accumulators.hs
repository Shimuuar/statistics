{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Statistics.Sample.Accumulators (
    -- * Generic API and type classes
    calculate
  , HasCount(..)
  , HasMean(..)
  , HasVariance(..)
  , HasMLVariance(..)
  , HasStdDev(..)
  , HasMLStdDev(..)
    -- * Accumulators
  , Count(..)
  , Binomial(..)
  , WelfordMean(..)
  , RobustVar(..)
  , robustVariance
    -- ** Summation
  ) where

import Control.Arrow
import Data.Monoid
import Data.Folds
import Data.Folds.MultiPass
import Numeric.Sum hiding (sum)


----------------------------------------------------------------
-- Generic API and type classes
----------------------------------------------------------------

-- | Calculate statistics from sample
calculate :: (Sample s, Accumulator m (Element s)) => (m -> a) -> s -> a
calculate f src = f $ runFold (fromAcc +<< toSource src)
{-# INLINE calculate #-}

class HasCount a where
  calcCount :: a -> Int

class HasMean a where
  calcMean :: a -> Double

class HasVariance a where
  calcVariance :: a -> Double

class HasMLVariance a where
  calcMLVariance :: a -> Double

class HasStdDev a where
  calcStdDev :: a -> Double

class HasMLStdDev a where
  calcMLStdDev :: a -> Double



----------------------------------------------------------------
-- Accumulators
----------------------------------------------------------------

-- | Accumulator for Bernoulli trials.
data Binomial = Binomial
  { binomTrials    :: {-# UNPACK #-} !Int
  , binomSuccesses :: {-# UNPACK #-} !Int
  }
  deriving (Show)

instance Monoid Binomial where
  mempty = Binomial 0 0
  Binomial n1 k1 `mappend` Binomial n2 k2 = Binomial (n1+n2) (k1+k2)

instance Accumulator Binomial Bool where
  snoc (Binomial n k) f = Binomial (n+1) (if f then k + 1 else k)
  cons = flip snoc



-- | Welford's mean
data WelfordMean a = WelfordMean
  { welfordCount :: {-# UNPACK #-} !Int
  , welfordMean  :: !a
  }

instance (Fractional a, a~a') => Accumulator (WelfordMean a) a' where
  snoc (WelfordMean n m) x = WelfordMean n' m'
    where
      m' = m + (x - m) / fromIntegral n'
      n' = n + 1
  cons = flip snoc

instance Fractional a => Monoid (WelfordMean a) where
  mempty = WelfordMean 0 0
  mappend !(WelfordMean n x) !(WelfordMean k y)
    = WelfordMean (n + k) ((x*n' + y*k') / (n' + k'))
    where
      n' = fromIntegral n
      k' = fromIntegral k

instance HasCount (WelfordMean a) where
  calcCount = welfordCount
instance HasMean (WelfordMean Double) where
  calcMean  = welfordMean


-- | Fast variance of sample which require single pass over data
data FastVariance = FastVariance
  { fastVarCount :: {-# UNPACK #-} !Int
  , fastVarMean  :: {-# UNPACK #-} !Double
  , fastVarSumSq :: {-# UNPACK #-} !Double
  }

instance Monoid FastVariance where
  mempty = FastVariance 0 0 0
  mappend (FastVariance n1 m1 s1) (FastVariance n2 m2 s2)
    = FastVariance n m (s1 + s2)
    where
      WelfordMean n m = WelfordMean n1 m1 <> WelfordMean n2 m2

instance Accumulator FastVariance Double where
  snoc (FastVariance n m s) x
    = FastVariance (n+1) m' s'
    where
      m' = m + d / fromIntegral n
      s' = s + d * (x - m')
      d  = x - m

instance HasCount FastVariance where
  calcCount = fastVarCount

instance HasMean FastVariance where
  calcMean = fastVarMean

instance HasVariance FastVariance where
  calcVariance fv
    | n > 1     = fastVarSumSq fv / fromIntegral (n - 1)
    | otherwise = 0
    where
      n = fastVarCount fv

instance HasMLVariance FastVariance where
  calcMLVariance fv
    | n > 1     = fastVarSumSq fv / fromIntegral n
    | otherwise = 0
    where
      n = fastVarCount fv

instance HasStdDev FastVariance where
  calcStdDev = sqrt . calcVariance
instance HasMLStdDev FastVariance where
  calcMLStdDev = sqrt . calcMLVariance


-- | Robust variance
data RobustVar = RobustVar
  { robustvarCount :: {-# UNPACK #-} !Int
  , robustvarMean  :: {-# UNPACK #-} !Double
  , robustvarSumSq :: {-# UNPACK #-} !Double
  }

robustVariance :: MFold Double RobustVar
robustVariance = do
  WelfordMean n m <- mfold fromAcc
  WelfordMean _ s <- mfold (fromAcc +<< (arr (\x -> let d = x - m in d*d) :: Pipette Double Double))
  return $ RobustVar n m s

instance HasCount RobustVar where
  calcCount = robustvarCount

instance HasMean RobustVar where
  calcMean = robustvarMean

instance HasVariance RobustVar where
  calcVariance fv
    | n > 1     = robustvarSumSq fv / fromIntegral (n - 1)
    | otherwise = 0
    where
      n = robustvarCount fv

instance HasMLVariance RobustVar where
  calcMLVariance fv
    | n > 1     = robustvarSumSq fv / fromIntegral n
    | otherwise = 0
    where
      n = robustvarCount fv

instance HasStdDev RobustVar where
  calcStdDev = sqrt . calcVariance
instance HasMLStdDev RobustVar where
  calcMLStdDev = sqrt . calcMLVariance




----------------------------------------------------------------
-- Orphans
----------------------------------------------------------------

instance Monoid KahanSum where
  mempty = zero
  KahanSum x c1 `mappend` KahanSum y c2 = KahanSum (x+y) (c1+c2)

instance Accumulator KahanSum Double where
  snoc = add
  cons = flip add


instance Monoid KBNSum where
  mempty = zero
  KBNSum x c1 `mappend` KBNSum y c2 = KBNSum (x+y) (c1+c2)

instance Accumulator KBNSum Double where
  snoc = add
  cons = flip add


instance Monoid KB2Sum where
  mempty = zero
  KB2Sum x c1 cc1 `mappend` KB2Sum y c2 cc2 = KB2Sum (x+y) (c1+c2) (cc1+cc2)

instance Accumulator KB2Sum Double where
  snoc = add
  cons = flip add

