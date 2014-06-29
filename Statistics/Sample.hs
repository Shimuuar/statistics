{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module    : Statistics.Sample
-- Copyright : (c) 2008 Don Stewart, 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Commonly used sample statistics, also known as descriptive
-- statistics.

module Statistics.Sample
    (
    -- * Types
      Sample
    , WeightedSample
    -- * Descriptive functions
    , range

    -- * Statistics of location
    , mean
    , welfordMean
    , meanWeighted
    , harmonicMean
    , geometricMean

    -- * Statistics of dispersion
    -- $variance

    -- ** Functions over central moments
    , centralMoment
    , skewness
    , kurtosis

    -- ** Two-pass functions (numerically robust)
    -- $robust
    , robustVariance
    , variance
    , varianceML
    , stdDev
    , varianceWeighted

    -- ** Single-pass functions (faster, less safe)
    -- $cancellation
    , fastVariance
    , fastVarianceUnbiased
    , fastStdDev

    -- * References
    -- $references
    ) where

import Statistics.Function (minMax)
import Statistics.Sample.Internal (robustSumVar, sum)
import Statistics.Types (WeightedSample)
import Statistics.Sample.Accumulators
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Data.Folds

-- Operator ^ will be overriden
import Prelude hiding ((^), sum)

-- | /O(n)/ Range. The difference between the largest and smallest
-- elements of a sample.
range :: (G.Vector v Double) => v Double -> Double
range s = hi - lo
    where (lo , hi) = minMax s
{-# INLINE range #-}

-- | /O(n)/ Arithmetic mean.  This uses Kahan-Babuška-Neumaier
-- summation, so is more accurate than 'welfordMean' unless the input
-- values are very large.
mean :: (Sample s, Element s ~ Double) => s -> Double
mean xs = runFold (calcMean +<< toSource xs)
{-# INLINE mean #-}
-- FIXME: We don't need to calculate length for vectors.


-- | /O(n)/ Arithmetic mean.  This uses Welford's algorithm to provide
-- numerical stability, using a single pass over the sample data.
--
-- Compared to 'mean', this loses a surprising amount of precision
-- unless the inputs are very large.
welfordMean :: (Sample s, Fractional (Element s)) => s -> Element s
welfordMean xs = getWelfordMean $ runFold (calcWelfordMean +<< toSource xs)
{-# INLINE welfordMean #-}


-- | /O(n)/ Arithmetic mean for weighted sample. It uses a single-pass
-- algorithm analogous to the one used by 'welfordMean'.
meanWeighted :: (G.Vector v (Double,Double)) => v (Double,Double) -> Double
meanWeighted = fini . G.foldl' go (V 0 0)
    where
      fini (V a _) = a
      go (V m w) (x,xw) = V m' w'
          where m' | w' == 0   = 0
                   | otherwise = m + xw * (x - m) / w'
                w' = w + xw
{-# INLINE meanWeighted #-}

-- | /O(n)/ Harmonic mean.  This algorithm performs a single pass over
-- the sample.
harmonicMean :: (Sample s, Element s ~ Double) => s -> Double
harmonicMean xs
  = getHarmonicMean
  $ runFold ((fromAcc :: Fold Double (HarmonicMean Mean)) +<< toSource xs)
{-# INLINE harmonicMean #-}

-- | /O(n)/ Geometric mean of a sample containing no negative values.
geometricMean :: (Sample s, Element s ~ Double) => s -> Double
geometricMean xs
  = getGeomMean
  $ runFold ((fromAcc :: Fold Double (GeometricMean Mean)) +<< toSource xs)
{-# INLINE geometricMean #-}


-- | Compute the /k/th central moment of a sample.  The central moment
-- is also known as the moment about the mean.
--
-- This function performs two passes over the sample, so is not subject
-- to stream fusion.
--
-- For samples containing many values very close to the mean, this
-- function is subject to inaccuracy due to catastrophic cancellation.
centralMoment :: (Sample s, Element s ~ Double) => Int -> s -> Double
centralMoment a xs
  = runFold (calcCentralMoment a +<< toSource xs)
{-# INLINE centralMoment #-}

-- | Compute the skewness of a sample. This is a measure of the
-- asymmetry of its distribution.
--
-- A sample with negative skew is said to be /left-skewed/.  Most of
-- its mass is on the right of the distribution, with the tail on the
-- left.
--
-- > skewness $ U.to [1,100,101,102,103]
-- > ==> -1.497681449918257
--
-- A sample with positive skew is said to be /right-skewed/.
--
-- > skewness $ U.to [1,2,3,4,100]
-- > ==> 1.4975367033335198
--
-- A sample's skewness is not defined if its 'variance' is zero.
--
-- This function performs two passes over the sample, so is not subject
-- to stream fusion.
--
-- For samples containing many values very close to the mean, this
-- function is subject to inaccuracy due to catastrophic cancellation.
skewness :: (Sample s, Element s ~ Double) => s -> Double
skewness xs = runFold (calcSkewness +<< toSource xs)
{-# INLINE skewness #-}

-- | Compute the excess kurtosis of a sample.  This is a measure of
-- the \"peakedness\" of its distribution.  A high kurtosis indicates
-- that more of the sample's variance is due to infrequent severe
-- deviations, rather than more frequent modest deviations.
--
-- A sample's excess kurtosis is not defined if its 'variance' is
-- zero.
--
-- This function performs two passes over the sample, so is not subject
-- to stream fusion.
--
-- For samples containing many values very close to the mean, this
-- function is subject to inaccuracy due to catastrophic cancellation.
kurtosis :: (Sample s, Element s ~ Double) => s -> Double
kurtosis xs = runFold (calcKurtosis +<< toSource xs)
{-# INLINE kurtosis #-}

-- $variance
--
-- The variance&#8212;and hence the standard deviation&#8212;of a
-- sample of fewer than two elements are both defined to be zero.

-- $robust
--
-- These functions use the compensated summation algorithm of Chan et
-- al. for numerical robustness, but require two passes over the
-- sample data as a result.
--
-- Because of the need for two passes, these functions are /not/
-- subject to stream fusion.

data V = V {-# UNPACK #-} !Double {-# UNPACK #-} !Double


-- | Calculate variance of a sample
robustVariance :: (Sample s, Element s ~ Double) => s -> RobustVar
robustVariance xs = runFold (calcRobustVariance +<< toSource xs)
{-# INLINE robustVariance #-}

-- | Unbiased estimate of a sample's variance. Also known
--   as the sample variance, where the denominator is /n-1/.
variance :: (Sample s, Element s ~ Double) => s -> Double
variance xs = getVariance $ runFold (calcRobustVariance +<< toSource xs)
{-# INLINE variance #-}

-- | Standard deviation.  This is simply the square root of the
-- unbiased estimate of the variance.
stdDev :: (Sample s, Element s ~ Double) => s -> Double
stdDev = sqrt . variance
{-# INLINE stdDev #-}

-- | Maximum likelihood estimate of a sample's variance. Also known as the
--   population variance, where the denominator is /n/.
varianceML :: (Sample s, Element s ~ Double) => s -> Double
varianceML xs = getVariance $ runFold (calcRobustVariance +<< toSource xs)
{-# INLINE varianceML #-}

robustSumVarWeighted :: (G.Vector v (Double,Double)) => v (Double,Double) -> V
robustSumVarWeighted samp = G.foldl' go (V 0 0) samp
    where
      go (V s w) (x,xw) = V (s + xw*d*d) (w + xw)
          where d = x - m
      m = meanWeighted samp
{-# INLINE robustSumVarWeighted #-}

-- | Weighted variance. This is biased estimation.
varianceWeighted :: (G.Vector v (Double,Double)) => v (Double,Double) -> Double
varianceWeighted samp
    | G.length samp > 1 = fini $ robustSumVarWeighted samp
    | otherwise         = 0
    where
      fini (V s w) = s / w
{-# INLINE varianceWeighted #-}

-- $cancellation
--
-- The functions prefixed with the name @fast@ below perform a single
-- pass over the sample data using Knuth's algorithm. They usually
-- work well, but see below for caveats. These functions are subject
-- to array fusion.
--
-- /Note/: in cases where most sample data is close to the sample's
-- mean, Knuth's algorithm gives inaccurate results due to
-- catastrophic cancellation.

fastVar :: (G.Vector v Double) => v Double -> T1
fastVar = G.foldl' go (T1 0 0 0)
  where
    go (T1 n m s) x = T1 n' m' s'
      where n' = n + 1
            m' = m + d / fromIntegral n'
            s' = s + d * (x - m')
            d  = x - m

-- | Maximum likelihood estimate of a sample's variance.
fastVariance :: (G.Vector v Double) => v Double -> Double
fastVariance = fini . fastVar
  where fini (T1 n _m s)
          | n > 1     = s / fromIntegral n
          | otherwise = 0
{-# INLINE fastVariance #-}

-- | Unbiased estimate of a sample's variance.
fastVarianceUnbiased :: (G.Vector v Double) => v Double -> Double
fastVarianceUnbiased = fini . fastVar
  where fini (T1 n _m s)
          | n > 1     = s / fromIntegral (n - 1)
          | otherwise = 0
{-# INLINE fastVarianceUnbiased #-}

-- | Standard deviation.  This is simply the square root of the
-- maximum likelihood estimate of the variance.
fastStdDev :: (G.Vector v Double) => v Double -> Double
fastStdDev = sqrt . fastVariance
{-# INLINE fastStdDev #-}

------------------------------------------------------------------------
-- Helper code. Monomorphic unpacked accumulators.

-- (^) operator from Prelude is just slow.
(^) :: Double -> Int -> Double
x ^ 1 = x
x ^ n = x * (x ^ (n-1))
{-# INLINE (^) #-}

data T1 = T1 {-# UNPACK #-}!Int {-# UNPACK #-}!Double {-# UNPACK #-}!Double

{-

Consider this core:

with data T a = T !a !Int

$wfold :: Double#
               -> Int#
               -> Int#
               -> (# Double, Int# #)

and without,

$wfold :: Double#
               -> Int#
               -> Int#
               -> (# Double#, Int# #)

yielding to boxed returns and heap checks.

-}

-- $references
--
-- * Chan, T. F.; Golub, G.H.; LeVeque, R.J. (1979) Updating formulae
--   and a pairwise algorithm for computing sample
--   variances. Technical Report STAN-CS-79-773, Department of
--   Computer Science, Stanford
--   University. <ftp://reports.stanford.edu/pub/cstr/reports/cs/tr/79/773/CS-TR-79-773.pdf>
--
-- * Knuth, D.E. (1998) The art of computer programming, volume 2:
--   seminumerical algorithms, 3rd ed., p. 232.
--
-- * Welford, B.P. (1962) Note on a method for calculating corrected
--   sums of squares and products. /Technometrics/
--   4(3):419&#8211;420. <http://www.jstor.org/stable/1266577>
--
-- * West, D.H.D. (1979) Updating mean and variance estimates: an
--   improved method. /Communications of the ACM/
--   22(9):532&#8211;535. <http://doi.acm.org/10.1145/359146.359153>
