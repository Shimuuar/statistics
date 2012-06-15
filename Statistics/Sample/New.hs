{-# LANGUAGE TypeFamilies #-}
-- |
-- More generic imlementation of functions from 'Statistics.Sample'
module Statistics.Sample.New (
  -- * Statistics of location
    mean
  , meanWeighted
  , harmonicMean
  , geometricMean
    -- * Statistics of dispersion
  , estVariance
  , fastVariance
  ) where

import Statistics.Sample.Classes
import Statistics.Sample.Estimators

----------------------------------------------------------------
-- Statistics of locations
----------------------------------------------------------------

-- | /O(n)/ Arithmetic mean.  This uses Welford's algorithm to provide
-- numerical stability, using a single pass over the sample data.
mean :: (Elem s ~ Double, Sample s) => s -> Double
mean = calcMean . estimateWith (E :: E MeanEst)
{-# INLINE mean #-}

-- | /O(n)/ Arithmetic mean for weighted sample. It uses a single-pass
-- algorithm analogous to the one used by 'mean'.
meanWeighted :: (Elem s ~ (Double,Double), Sample s) => s -> Double
meanWeighted = calcMean . estimateWith (E :: E MeanEst)
{-# INLINE meanWeighted #-}

-- | /O(n)/ Harmonic mean.  This algorithm performs a single pass over
-- the sample.
harmonicMean :: (Elem s ~ Double, Sample s) => s -> Double
harmonicMean = calcHarmonicMean . estimateWith (E :: E HarmonicMeanEst)
{-# INLINE harmonicMean #-}

-- | /O(n)/ Geometric mean of a sample containing no negative values.
geometricMean :: (Elem s ~ Double, Sample s) => s -> Double
geometricMean = calcGeometricMean . estimateWith (E :: E GeometricMeanEst)
{-# INLINE geometricMean #-}

----------------------------------------------------------------
-- Statistics of dispersion
----------------------------------------------------------------

estVariance :: (Elem s ~ Double, Sample s) => s -> VarianceEst
estVariance xs
  -- FIXME: count in fastvar and weight in Mean
  = VarianceEst n m (v * fromIntegral n)
  where
    (Count n, Mean m) = estimateWith (E :: E MeanEst)        xs
    Variance v        = estimateWith (E :: E RobustVariance) xs m

fastVariance :: (Elem s ~ Double, Sample s) => s -> VarianceEst
fastVariance = evalStatistics
{-# INLINE fastVariance #-}
