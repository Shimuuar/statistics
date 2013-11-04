{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}

{-# LANGUAGE UndecidableInstances #-}
module Statistics.Sample.Estimators (
    -- * Weighted
    Weigthed(..)
    -- * Statistics
  , Count(..)
  , Min(..)
  , Max(..)
    -- ** Statistics of location
  , Mean(..)
  , Median(..)
  , GeometricMean(..)
  , HarmonicMean(..)
    -- ** Statistics of dispesion
  , Variance(..)
  , StdDev(..)
  , VarianceBiased(..)
  , StdDevBiased(..)
    -- * Statistics of distribution shape
  , Skewness(..)
  , Kurtosis(..)

    -- * Estimators
  , CountEst(..)
  , MinEst(..)
  , MaxEst(..)
    -- ** Sample mean and variance
  , MeanEst(..)
  , GeometricMeanEst(..)
  , HarmonicMeanEst(..)
    -- ** Variance
  , RobustVariance(..)
  , VarianceEst(..)
    -- * Transformers
  , SkipNaN(..)
  ) where

import Data.Data (Typeable,Data)

import Statistics.Sample.Classes

----------------------------------------------------------------
--
----------------------------------------------------------------

data Weigthed w a = Weigthed w a

----------------------------------------------------------------
-- Statistics
----------------------------------------------------------------

-- | Number of elements in the sample.
newtype Count = Count { calcCount :: Int }
                deriving (Eq,Show,Typeable,Data)

-- | Minimal value in the sample.
newtype Min a = Min { calcMin :: a }
                deriving (Eq,Show,Typeable,Data)

-- | Maximal value in the sample.
newtype Max a = Max { calcMax :: a }
                deriving (Eq,Show,Typeable,Data)



-- Statistics of location --------------------------------------

-- | Arithmetic mean of the sample.
newtype Mean = Mean { calcMean :: Double }
               deriving (Eq,Show,Typeable,Data)

-- | Median of the sample.
newtype Median = Median { calcMedian :: Double }
               deriving (Eq,Show,Typeable,Data)

-- | Geometric mean of the sample.
newtype GeometricMean = GeometricMean { calcGeometricMean :: Double }
               deriving (Eq,Show,Typeable,Data)

newtype HarmonicMean = HarmonicMean { calcHarmonicMean :: Double }
               deriving (Eq,Show,Typeable,Data)



-- Statistics of dispersion ------------------------------------


-- | Unbiased estimate of variance. Also known as sample variance,
--   where the denominator is /n/-1.
newtype Variance = Variance { calcVariance :: Double }
               deriving (Eq,Show,Typeable,Data)

-- | Standard deviation computed from unbiased estimate of variance.
newtype StdDev = StdDev { calcStdDev :: Double }
               deriving (Eq,Show,Typeable,Data)

-- | Maximum likelihood estimate of a sample's variance. Also known
--   as the population variance, where the denominator is /n/.
newtype VarianceBiased = VarianceBiased { calcVarianceBiased :: Double }
                       deriving (Eq,Show,Typeable,Data)

-- | Standard deviation computed from likelihood estimate of variance.
newtype StdDevBiased = StdDevBiased { calcStdDevBiased :: Double }
                     deriving (Eq,Show,Typeable,Data)

-- | This is a measure of the asymmetry of its distribution.  A sample
--   with negative skew is said to be /left-skewed/. Most of its mass
--   is on the right of the distribution, with the tail on the left.
--
-- > skewness [1,100,101,102,103] = -1.497681449918257
--
--   A sample with positive skew is said to be /right-skewed/.
--
-- > skewness [1,2,3,4,100] = 1.4975367033335198
--
--   A sample's skewness is not defined if its 'variance' is zero.
newtype Skewness = Skewness { calcSkewness :: Double }
                 deriving (Eq,Show,Typeable,Data)

-- | This is a measure of the \"peakedness\" of its distribution.  A
--   high kurtosis indicates that more of the sample's variance is due
--   to infrequent severe deviations, rather than more frequent modest
--   deviations.
--
--   A sample's excess kurtosis is not defined if its 'variance' is
--   zero.
newtype Kurtosis = Kurtosis { calcKurtosis :: Double }
                 deriving (Eq,Show,Typeable,Data)


-- Instances
instance Calc m Variance => Calc m StdDev where
  calc = StdDev . sqrt . calcVariance . calc

instance Calc m VarianceBiased => Calc m StdDevBiased where
  calc = StdDevBiased . sqrt . calcVarianceBiased . calc



----------------------------------------------------------------
-- Estimators
----------------------------------------------------------------

-- | Count number of elements in the sample
newtype CountEst = CountEst Int

instance FoldEstimator CountEst a where
  addElement (CountEst x) _ = CountEst (x+1)
  {-# INLINE addElement #-}

instance NullEstimator CountEst where
  nullEstimator   = CountEst 0
  {-# INLINE nullEstimator #-}

instance MonoidEst CountEst where
  mergeSamples (CountEst n) (CountEst m) = CountEst (n+m)
  {-# INLINE mergeSamples #-}

instance Calc CountEst Count where
  calc (CountEst n) = Count n
  {-# INLINE calc #-}



----------------------------------------------------------------

-- | Find minimal element in the sample
newtype MinEst a = MinEst a
                deriving (Show,Eq,Typeable,Data)
-- FIXME:
{-
instance Ord a => NonEmptyEst (MinEst a) a where
  nonemptyEst = Init $ Est . MinEst
  {-# INLINE nonemptyEst #-}

instance Ord a => FoldEstimator (MinEst a) a where
  addElement (MinEst a) b = MinEst $ min a b
  {-# INLINE addElement #-}

instance Ord a => MonoidEst (MinEst a) where
  mergeSamples (MinEst a) (MinEst b) = MinEst (min a b)
  {-# INLINE mergeSamples #-}

instance a ~ a' => Calc (MinEst a) (Min a') where
  calc (MinEst x) = Min x
  {-# INLINE calc #-}
-}


----------------------------------------------------------------

-- | Find maximal element in the sample
newtype MaxEst a = MaxEst a
                deriving (Show,Eq,Typeable,Data)
-- FIXME:
{-
instance Ord a => NonEmptyEst (MaxEst a) a where
  nonemptyEst = Init $ Est . MaxEst
  {-# INLINE nonemptyEst #-}

instance Ord a => FoldEstimator (MaxEst a) a where
  addElement (MaxEst a) b = MaxEst $ max a b
  {-# INLINE addElement #-}

instance Ord a => MonoidEst (MaxEst a) where
  mergeSamples (MaxEst a) (MaxEst b) = MaxEst (max a b)
  {-# INLINE mergeSamples #-}

instance a ~ a' => Calc (MaxEst a) (Max a') where
  calc (MaxEst x) = Max x
  {-# INLINE calc #-}
-}


----------------------------------------------------------------

-- | Estimator for geometric mean. Sample must not contain
--   non-positive elements otherwise algorithm will return nonsentical
--   value.
--
--   For empty sample mean is set to 1.
data GeometricMeanEst = GeometricMeanEst
                        {-# UNPACK #-} !Double
                        {-# UNPACK #-} !Int

instance NullEstimator GeometricMeanEst where
  nullEstimator = GeometricMeanEst 1 0
  {-# INLINE nullEstimator #-}

instance FoldEstimator GeometricMeanEst Double where
  addElement (GeometricMeanEst x n) a = GeometricMeanEst (x * a) (n + 1)
  {-# INLINE addElement #-}

instance MonoidEst GeometricMeanEst where
  mergeSamples (GeometricMeanEst x n) (GeometricMeanEst y m) = GeometricMeanEst (x * y) (n + m)
  {-# INLINE mergeSamples #-}

instance Calc GeometricMeanEst Count where
  calc (GeometricMeanEst _ n) = Count n

instance Calc GeometricMeanEst GeometricMean where
  calc (GeometricMeanEst x n) = GeometricMean $ x ** (1 / fromIntegral n)



----------------------------------------------------------------
-- | Estimator for harmonic mean. Sample must not contain
--   non-positive elements otherwise algorithm will return nonsentical
--   value.
--
--   For empty sample mean is set to 1.
data HarmonicMeanEst = HarmonicMeanEst
                        {-# UNPACK #-} !Double
                        {-# UNPACK #-} !Int

instance NullEstimator HarmonicMeanEst where
  nullEstimator = HarmonicMeanEst 0 0
  {-# INLINE nullEstimator #-}

instance FoldEstimator HarmonicMeanEst Double where
  addElement (HarmonicMeanEst x n) a = HarmonicMeanEst (x + 1 / a) (n + 1)
  {-# INLINE addElement #-}

instance MonoidEst HarmonicMeanEst where
  mergeSamples (HarmonicMeanEst x n) (HarmonicMeanEst y m) = HarmonicMeanEst (x * y) (n + m)
  {-# INLINE mergeSamples #-}

instance Calc HarmonicMeanEst Count where
  calc (HarmonicMeanEst _ n) = Count n

instance Calc HarmonicMeanEst HarmonicMean where
  calc (HarmonicMeanEst x n) = HarmonicMean $ fromIntegral n / x



----------------------------------------------------------------

-- | Accumulator for sample mean. It uses Welford's algorithm to
--   provide numerical stability.
--
--   For elements of type 'Double' it calculates mean and for elements
--   of type '(Double,Double)' it calculates weighted mean. Weight is
--   second element of the tuple.
data MeanEst = MeanEst
               {-# UNPACK #-} !Double -- Get mean estimate from accumulator.
               {-# UNPACK #-} !Double
               -- Get summary weight of elements. For unweighted mean it equal
               -- to total number of elements.
             deriving (Eq,Show,Typeable,Data)

instance FoldEstimator MeanEst Double where
  addElement (MeanEst m n) x = MeanEst m' n'
    where
      m' = m + (x - m) / n'
      n' = n + 1
  {-# INLINE addElement #-}
instance FoldEstimator MeanEst (Double,Double) where
  addElement e@(MeanEst m n) (x, w)
    | w == 0    = e
    | otherwise = MeanEst m' n'
    where
      n' = n + w
      m' = m + w * (x - m) / n'
  {-# INLINE addElement #-}

instance NullEstimator MeanEst where
  nullEstimator = MeanEst 0 0
  {-# INLINE nullEstimator #-}

instance MonoidEst MeanEst where
   mergeSamples a@(MeanEst x n) b@(MeanEst y k)
     | n == 0    = a
     | k == 0    = b
     | otherwise = MeanEst ((x*n + y*k) / s) s
     where s = n + k
   {-# INLINE mergeSamples #-}

instance Calc MeanEst Mean where
  calc (MeanEst m _) = Mean m

-- FIXME: utterly wrong!
instance Calc MeanEst Count where
  calc (MeanEst _ n) = Count $ round n


----------------------------------------------------------------

-- FIXME: check estimates for weighted events. Really important!

-- FIXME: we need to guarantee that parameter to the mean estimator is
--        always floated out. Otherwise we would face space leak

-- | Robust estimator for variance
newtype RobustVariance = RobustVariance (Double -> MeanEst)

instance FoldEstimator RobustVariance Double where
  addElement (RobustVariance f) x = RobustVariance $ \m -> addElement (f m) (let d = x - m in d*d)
  {-# INLINE addElement #-}

instance NullEstimator RobustVariance where
  nullEstimator = RobustVariance $ \_ -> nullEstimator
  {-# INLINE nullEstimator #-}

instance MonoidEst RobustVariance where
  mergeSamples (RobustVariance f) (RobustVariance g) = RobustVariance $ \m -> mergeSamples (f m) (g m)
  {-# INLINE mergeSamples #-}

instance Calc RobustVariance (Double -> Variance) where
  calc (RobustVariance est) m =
    case est m of
      MeanEst x n
        | n < 2     -> Variance   0
        | otherwise -> Variance $ x * n / (n - 1)
  {-# INLINE calc #-}

instance Calc RobustVariance (Double -> VarianceBiased) where
  calc (RobustVariance est) m =
    case est m of MeanEst x _ -> VarianceBiased x
  {-# INLINE calc #-}

instance Calc RobustVariance (Double -> StdDev) where
  calc est = StdDev . calcVariance . calc est
  {-# INLINE calc #-}

instance Calc RobustVariance (Double -> StdDevBiased) where
  calc est = StdDevBiased . calcVariance . calc est
  {-# INLINE calc #-}

-- Eek! Undecidable
instance Calc RobustVariance (Double -> r) => Calc RobustVariance (Mean -> r) where
  calc est (Mean m) = calc est m


----------------------------------------------------------------

data VarianceEst = VarianceEst
               {-# UNPACK #-} !Int
               {-# UNPACK #-} !Double
               {-# UNPACK #-} !Double
             deriving (Eq,Show,Typeable)


instance FoldEstimator VarianceEst Double where
  addElement (VarianceEst n m s) x = VarianceEst n' m' s'
    where
      n' = n + 1
      m' = m + d / fromIntegral n'
      s' = s + d * (x - m')
      d  = x - m
  {-# INLINE addElement #-}

instance NullEstimator VarianceEst where
  nullEstimator = VarianceEst 0 0 0
  {-# INLINE nullEstimator #-}

instance Calc VarianceEst Mean where
  calc (VarianceEst _ m _) = Mean m

instance Calc VarianceEst Count where
  calc (VarianceEst n _ _) = Count n

instance Calc VarianceEst Variance where
  calc (VarianceEst n _ s)
    | n > 1     = Variance (s / fromIntegral (n - 1))
    | otherwise = Variance  0

instance Calc VarianceEst VarianceBiased where
  calc (VarianceEst n _ s)
    | n > 1     = VarianceBiased (s / fromIntegral n)
    | otherwise = VarianceBiased  0



----------------------------------------------------------------

-- | Skip NaN in calculation of statistics
newtype SkipNaN m = SkipNaN { skipNaN :: m }
                    deriving (Show,Eq,Typeable)

instance (FoldEstimator m a, RealFloat a) => FoldEstimator (SkipNaN m) a where
  addElement (SkipNaN m) x
    | isNaN x   = SkipNaN   m
    | otherwise = SkipNaN $ addElement m x
  {-# INLINE addElement #-}

instance NullEstimator m => NullEstimator (SkipNaN m) where
  nullEstimator = SkipNaN nullEstimator
  {-# INLINE nullEstimator #-}

instance MonoidEst m => MonoidEst (SkipNaN m) where
  mergeSamples (SkipNaN m) (SkipNaN n) = SkipNaN (mergeSamples m n)
  {-# INLINE mergeSamples #-}

instance Calc m r => Calc (SkipNaN m) r where
  calc = calc . skipNaN
