{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Statistics.Sample.Accumulators (
    -- * Generic API and type classes
    calculate
    -- ** Type classes for statistics
  , HasCount(..)
  , HasMean(..)
  , HasGeometricMean(..)
  , HasHarmonicMean(..)
  , HasVariance(..)
  , HasMLVariance(..)
  , HasStdDev(..)
  , HasMLStdDev(..)
    -- * Accumulators
  , Count(Count)
  , Binomial(..)
  , Mean(..)
  , calcMean
  , WelfordMean(..)
  , calcWelfordMean
  , GeometricMean
  , HarmonicMean
  , RobustVar(..)
  , calcRobustVariance
  , FastVariance(..)
  , calcFastVariance
  , calcCentralMoment
  , calcSkewness
  , calcKurtosis
    -- ** Summation
  , KBNSum(..)
  ) where

import Control.Applicative
import Data.Monoid
import Data.Folds hiding (getCount)
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
  getCount :: a -> Int

class HasMean a where
  getMean :: a -> Double

class HasGeometricMean a where
  getGeomMean :: a -> Double

class HasHarmonicMean a where
  getHarmonicMean :: a -> Double

class HasVariance a where
  getVariance :: a -> Double

class HasMLVariance a where
  getMLVariance :: a -> Double

class HasStdDev a where
  getStdDev :: a -> Double

class HasMLStdDev a where
  getMLStdDev :: a -> Double



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


-- | Numerically stable accumulator fro mean
data Mean = Mean
  { stableCount :: {-# UNPACK #-} !Int
  , stableSum   :: {-# UNPACK #-} !KBNSum
  }

instance Accumulator Mean Double where
  snoc (Mean n s) x = Mean (n+1) (snoc s x)
  cons = flip snoc

instance HasCount Mean where
  getCount = stableCount
instance HasMean Mean where
  getMean m = kbn (stableSum m) / fromIntegral (stableCount m)

instance Monoid Mean where
  mempty = Mean 0 mempty
  Mean n1 m1 `mappend` Mean n2 m2 = Mean (n1+n2) (m1 <> m2)

-- | Evaluate mean using fold
calcMean :: Fold Double Double
calcMean = (getMean :: Mean -> Double) <$> fromAcc


-- | Welford's mean
data WelfordMean a = WelfordMean
  { getWelfordCount :: {-# UNPACK #-} !Int
  , getWelfordMean  :: !a
  }

calcWelfordMean :: Fractional a => Fold a (WelfordMean a)
calcWelfordMean = fromAcc
{-# INLINE calcWelfordMean #-}

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
  getCount = getWelfordCount
instance HasMean (WelfordMean Double) where
  getMean  = getWelfordMean

-- | Geometric mean. It's parametrized by actual mean implementation
newtype GeometricMean m = GeometricMean m

instance Monoid m => Monoid (GeometricMean m) where
  mempty = GeometricMean mempty
  mappend (GeometricMean m1) (GeometricMean m2) = GeometricMean (mappend m1 m2)

instance (Floating a, Accumulator m a) => Accumulator (GeometricMean m) a where
  snoc (GeometricMean m) x = GeometricMean (snoc m (log x))
  cons x (GeometricMean m) = GeometricMean (cons (log x) m)
  unit x = GeometricMean (unit x)

instance HasCount m => HasCount (GeometricMean m) where
  getCount (GeometricMean m) = getCount m

instance HasMean m => HasGeometricMean (GeometricMean m) where
  getGeomMean (GeometricMean m) = exp (getMean m)


-- | Harmonic mean
newtype HarmonicMean m = HarmonicMean m

instance Monoid m => Monoid (HarmonicMean m) where
  mempty = HarmonicMean mempty
  mappend (HarmonicMean a) (HarmonicMean b) = HarmonicMean (mappend a b)

instance (Fractional a, Accumulator m a) => Accumulator (HarmonicMean m) a where
  snoc (HarmonicMean m) x = HarmonicMean $ snoc m (recip x)
  cons x (HarmonicMean m) = HarmonicMean $ cons (recip x) m
  unit x = HarmonicMean (unit $ recip x)


instance HasCount m => HasCount (HarmonicMean m) where
  getCount (HarmonicMean m) = getCount m

instance HasMean m => HasHarmonicMean (HarmonicMean m) where
  getHarmonicMean (HarmonicMean m) = recip (getMean m)


-- | Fast variance of sample which require single pass over data.
data FastVariance = FastVariance
  { fastVarCount :: {-# UNPACK #-} !Int
  , fastVarMean  :: {-# UNPACK #-} !Double
  , fastVarSumSq :: {-# UNPACK #-} !Double
  }

calcFastVariance :: Fold Double FastVariance
calcFastVariance = fromAcc
{-# INLINE calcFastVariance #-}

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
  getCount = fastVarCount

instance HasMean FastVariance where
  getMean = fastVarMean

instance HasVariance FastVariance where
  getVariance fv
    | n > 1     = fastVarSumSq fv / fromIntegral (n - 1)
    | otherwise = 0
    where
      n = fastVarCount fv

instance HasMLVariance FastVariance where
  getMLVariance fv
    | n > 1     = fastVarSumSq fv / fromIntegral n
    | otherwise = 0
    where
      n = fastVarCount fv

instance HasStdDev FastVariance where
  getStdDev = sqrt . getVariance
instance HasMLStdDev FastVariance where
  getMLStdDev = sqrt . getMLVariance


-- | Robust variance
data RobustVar = RobustVar
  { robustvarCount :: {-# UNPACK #-} !Int
  , robustvarMean  :: {-# UNPACK #-} !Double
  , robustvarSumSq :: {-# UNPACK #-} !Double
  }

-- | Calculate variance of sample using multiple pass fold.
calcRobustVariance :: MFold Double RobustVar
calcRobustVariance = do
  WelfordMean n m <- mfold fromAcc
  WelfordMean _ s <- mfold (fromAcc +<< pipe (\x -> let d = x - m in d*d))
  return $ RobustVar n m s

instance HasCount RobustVar where
  getCount = robustvarCount

instance HasMean RobustVar where
  getMean = robustvarMean

instance HasVariance RobustVar where
  getVariance fv
    | n > 1     = robustvarSumSq fv / fromIntegral (n - 1)
    | otherwise = 0
    where
      n = robustvarCount fv

instance HasMLVariance RobustVar where
  getMLVariance fv
    | n > 1     = robustvarSumSq fv / fromIntegral n
    | otherwise = 0
    where
      n = robustvarCount fv

instance HasStdDev RobustVar where
  getStdDev = sqrt . getVariance
instance HasMLStdDev RobustVar where
  getMLStdDev = sqrt . getMLVariance

-- | Calculate N-th central moment of sample
calcCentralMoment :: Int -> MFold Double Double
calcCentralMoment n
  | n <  0    = error "Statistics.Sample.Accumulators.calcCentralMoment: negative input"
  | n == 0    = pure 1
  | n == 1    = pure 0
  | otherwise = do m <- mfold calcMean
                   mfold (calcCentralMomentMean n m)

-- | Calculate N-th central moment of sample. It takes estimate of
--   mean as parameter.
calcCentralMomentMean :: Int -> Double -> Fold Double Double
calcCentralMomentMean n m =
  calcMean +<< pipe (\x -> let d = x - m in d^n)

-- | Calculate skewness of sample
calcSkewness :: MFold Double Double
calcSkewness = do
  m <- mfold calcMean
  mfold ((\c2 c3 -> c3 * c2**(-1.5))
         <$> calcCentralMomentMean 2 m
         <*> calcCentralMomentMean 3 m)
-- FIXME: we have two counter for sample size. Wasteful

-- | Calculate kurtosis of sample
calcKurtosis :: MFold Double Double
calcKurtosis = do
  m <- getWelfordMean <$> mfold fromAcc
  mfold ((\c2 c4 -> c4 / (c2*c2) - 3)
         <$> calcCentralMomentMean 2 m
         <*> calcCentralMomentMean 4 m)



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
