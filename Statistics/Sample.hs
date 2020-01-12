{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards    #-}
{-# LANGUAGE RankNTypes       #-}
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

module Statistics.Sample (
  -- * Descriptive functions
    stableSumOf
  , reduceSampleOf
  , minMaxOf
  , rangeOf
    -- * Statistics of location
  , meanOf
  , meanEstOf
  , weightedMeanOf
  , harmonicMeanOf
  , geometricMeanOf
    -- * Statistics of dispersion
    -- $variance
    -- -- ** Two-pass functions (numerically robust)
    -- $robust
  , varianceOf
  , varianceMLOf
  , stdDevOf
  , stdDevMLOf
  , stdErrMeanOf
    -- ** Functions over central moments
  , centralMomentOf
  , centralMomentsOf
  , skewnessOf
  , kurtosisOf
    -- * Covariance
  , covarianceOf
  , correlationOf
    -- * References
    -- $references
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad       (liftM)
import Control.Monad.Catch (MonadThrow(..))
import Statistics.Function (square)
import Statistics.Types.Internal  (StatisticsException(..))
import Statistics.Monoid.Class
import Statistics.Monoid.Numeric
import Data.Monoid         (Endo(..))
import qualified Numeric.Sum as Sum
import Prelude hiding ((^), sum) -- Operator ^ will be overridden

----------------------------------------------------------------
-- Data types
----------------------------------------------------------------


data SampleMean = SampleMean !Int !Double

instance CalcCount SampleMean where
  calcCount (SampleMean n _) = n

instance CalcMean SampleMean where
  calcMean  (SampleMean _ m) = Just m
instance HasMean SampleMean where
  getMean   (SampleMean _ m) = m

data SampleVariance = SampleVariance !Int !Double !Double

instance CalcCount SampleVariance where
  calcCount (SampleVariance n _ _) = n

instance CalcMean SampleVariance where
  calcMean  (SampleVariance _ m _) = Just m
instance HasMean SampleVariance where
  getMean   (SampleVariance _ m _) = m

instance CalcVariance SampleVariance where
  calcVariance   (SampleVariance n _ s) = Just $! s / fromIntegral n
  calcVarianceML (SampleVariance n _ s) = Just $! s / fromIntegral (n - 1)
instance HasVariance SampleVariance where
  getVariance   (SampleVariance n _ s) = s / fromIntegral n
  getVarianceML (SampleVariance n _ s) = s / fromIntegral (n - 1)



----------------------------------------------------------------
-- Estimators
----------------------------------------------------------------

-- | /O(n)/ Numerically stable sum. It uses KBN algorithm for
--   compensated summation.
stableSumOf :: Getting (Endo (Endo Sum.KBNSum)) s Double -> s -> Double
{-# INLINE stableSumOf #-}
stableSumOf l = Sum.kbn . foldlOf' l Sum.add Sum.zero

-- | /O(n)/ Compute statistics for sample using some monoidal
--   accumulator. For examle mean could be defined as:
--
-- > meanOf = calcMean . asMeanKBN . reduceSampleOf
reduceSampleOf :: (StatMonoid m a) => Getting (Endo (Endo m)) s a -> s -> m
{-# INLINE reduceSampleOf #-}
reduceSampleOf l = foldlOf' l addValue mempty

-- | /O(n)/ Compute minimum and maximum value of sample.
minMaxOf
  :: (Ord a, MonadThrow m)
  => Getting (Endo (Endo (Pair (Min a) (Max a)))) s a
  -> s -> m (a,a)
{-# INLINE minMaxOf #-}
minMaxOf l xs
  = liftErr "minMaxOf" "Empty sample"
  $ liftA2 (,) (calcMin minAcc) (calcMax maxAcc)
  where
    Pair minAcc maxAcc = reduceSampleOf l xs

-- | /O(n)/ Range. The difference between the largest and smallest
--   elements of a sample.
rangeOf
  :: (Ord a, Num a, MonadThrow m)
  => Getting (Endo (Endo (Pair (Min a) (Max a)))) s a
  -> s -> m a
{-# INLINE rangeOf #-}
rangeOf l xs = do
  (x1,x2) <- minMaxOf l xs
  return $! x2 - x1

-- | /O(n)/ Arithmetic mean.  This uses Kahan-Babuška-Neumaier
--   summation.
meanOf :: (Real a, MonadThrow m)
       => Getting (Endo (Endo MeanKBN)) s a -> s -> m Double
{-# INLINE meanOf #-}
meanOf l xs
  = liftErr "meanOf" "Empty sample"
  $ calcMean $ asMeanKBN $ reduceSampleOf l xs

-- | /O(n)/ Arithmetic mean.  This uses Kahan-Babuška-Neumaier
--   summation.
meanEstOf :: (Real a, MonadThrow m)
          => Getting (Endo (Endo MeanKBN)) s a -> s -> m SampleMean
{-# INLINE meanEstOf #-}
meanEstOf l xs
  = liftErr "meanEstOf" "Empty sample"
  $ do let acc = asMeanKBN $ reduceSampleOf l xs
       SampleMean (calcCount acc) <$> calcMean acc

-- | /O(n)/ Arithmetic mean for weighted sample. It uses a single-pass
--   algorithm analogous to the one used by 'welfordMean'.
weightedMeanOf
  :: (Real w, Real a, MonadThrow m)
  => Getting (Endo (Endo WMeanKBN)) s (Weighted w a) -> s -> m Double
{-# INLINE weightedMeanOf #-}
weightedMeanOf l xs
  = liftErr "weightedMeanOf" "Empty sample"
  $ calcMean $ asWMeanKBN $ reduceSampleOf l xs

-- | /O(n)/ Harmonic mean.
harmonicMeanOf
  :: (MonadThrow m, Real a, Fractional a)
  => Getting (Endo (Endo MeanKBN)) s a -> s -> m Double
{-# INLINE harmonicMeanOf #-}
harmonicMeanOf l xs
  = liftErr "harmonicMeanOf" "Empty sample"
  $ do m <- calcMean acc
       return $! fromIntegral (calcCount acc) / m
  where
    acc = asMeanKBN $ reduceSampleOf (l . to recip) xs

-- | /O(n)/ Geometric mean of a sample containing no negative values.
geometricMeanOf
  :: (MonadThrow m, RealFloat a)
  => Getting (Endo (Endo MeanKBN)) s a -> s -> m Double
{-# INLINE geometricMeanOf #-}
geometricMeanOf l
  = liftErr "geometricMeanOf" "Empty sample"
  . fmap exp
  . meanOf (l . to log)

-- | Compute the /k/th central moment of a sample.  The central moment
-- is also known as the moment about the mean.
--
-- This function performs two passes over the sample, so is not subject
-- to stream fusion.
--
-- For samples containing many values very close to the mean, this
-- function is subject to inaccuracy due to catastrophic cancellation.
centralMomentOf
  :: (MonadThrow m)
  => Int       -- ^ Central moment to compute. Must be nonnegative
  -> Getting (Endo (Endo MeanKBN)) s Double
  -> s
  -> m Double
{-# INLINE centralMomentOf #-}
centralMomentOf a l xs
  | a < 0     = modErr "centralMomentOf" "Negative central moment"
  | a == 0    = return 1
  | a == 1    = return 0
  | otherwise = do m <- meanOf l xs
                   let cmoment x = (x - m) ^ a
                   meanOf (l . to cmoment) xs

-- | Compute the /k/th and /j/th central moments of a sample.
--
-- This function performs two passes over the sample, so is not subject
-- to stream fusion.
--
-- For samples containing many values very close to the mean, this
-- function is subject to inaccuracy due to catastrophic cancellation.
centralMomentsOf
  :: (MonadThrow m)
  => Int       -- ^ Central moment to compute. Must be nonnegative
  -> Int       -- ^ Another central moments
  -> (forall r. Getting (Endo (Endo r)) s Double)
  -> s
  -> m (Double,Double)
{-# INLINE centralMomentsOf #-}
centralMomentsOf a b l xs
  | a < 2 || b < 2 = do !cA <- centralMomentOf a l xs
                        !cB <- centralMomentOf b l xs
                        return (cA,cB)
  | otherwise      = do est <- meanEstOf l xs
                        let n = fromIntegral $ calcCount est
                            m = getMean est
                            step (V i j) x = V (Sum.add i (d^a)) (Sum.add j (d^b))
                              where d = x - m
                            fini (V i j) = (Sum.kbn i / n , Sum.kbn j / n)
                        return $! fini $ foldlOf' l step (V mempty mempty) xs

data V = V {-# UNPACK #-} !Sum.KBNSum {-# UNPACK #-} !Sum.KBNSum


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
skewnessOf
  :: (MonadThrow m)
  => (forall r. Getting (Endo (Endo r)) s Double)
  -> s
  -> m Double
{-# INLINE skewnessOf #-}
skewnessOf l xs = do
  (c3 , c2) <- centralMomentsOf 3 2 l xs
  return $! c3 * c2 ** (-1.5)


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
kurtosisOf
  :: (MonadThrow m)
  => (forall r. Getting (Endo (Endo r)) s Double)
  -> s
  -> m Double
{-# INLINE kurtosisOf #-}
kurtosisOf l xs = do
  (c4 , c2) <- centralMomentsOf 4 2 l xs
  return $! c4 / (c2 * c2) - 3

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


varianceEstOf
  :: (MonadThrow m)
  => (forall r. Getting (Endo (Endo r)) s Double) -> s -> m SampleVariance
{-# INLINE varianceEstOf #-}
varianceEstOf l xs
  | Just est <- meanEstOf l xs
  , calcCount est > 1
    = let m = getMean est
          s = stableSumOf (l . to (subtract m >> square)) xs
      in return $! SampleVariance (calcCount est) m s
  | otherwise
    = modErr "varianceEstOf" "Insufficient sample size"

-- | Unbiased estimate of a sample's variance. Also known as the
--   sample variance
--
--   \[ \sigma^2 = \frac{1}{N-1}\sum_{i=1}^N(x_i - \bar{x})^2 \]
varianceOf
  :: (MonadThrow m)
  => (forall r. Getting (Endo (Endo r)) s Double) -> s -> m Double
{-# INLINE varianceOf #-}
varianceOf l
  = liftM getVariance
  . varianceEstOf l

-- | Maximum likelihood estimate of a sample's variance. Also known
--   as the population variance:
--
--   \[ \sigma^2 = \frac{1}{N}\sum_{i=1}^N(x_i - \bar{x})^2 \]
varianceMLOf
  :: (MonadThrow m)
  => (forall r. Getting (Endo (Endo r)) s Double) -> s -> m Double
{-# INLINE varianceMLOf #-}
varianceMLOf l
  = liftM getVarianceML
  . varianceEstOf l

-- | Standard deviation. This is simply the square root of the
--   unbiased estimate of the variance. Note that this estimate is not
--   unbiased itself.
--
--   \[ \sigma = \sqrt{\frac{1}{N-1}\sum_{i=1}^N(x_i - \bar{x})^2 } \]
-- stdDevOf :: (G.Vector v Double, MonadThrow m) => v Double -> m Double
stdDevOf
  :: (MonadThrow m)
  => (forall r. Getting (Endo (Endo r)) s Double) -> s -> m Double
{-# INLINE stdDevOf #-}
stdDevOf l = liftM sqrt . varianceOf l

-- | Standard deviation. This is simply the square root of the
--   maximum likelihood estimate of the variance.
--
--   \[ \sigma = \sqrt{\frac{1}{N-1}\sum_{i=1}^N(x_i - \bar{x})^2 } \]
stdDevMLOf
  :: (MonadThrow m)
  => (forall r. Getting (Endo (Endo r)) s Double) -> s -> m Double
{-# INLINE stdDevMLOf #-}
stdDevMLOf l = liftM sqrt . varianceMLOf l

-- | Standard error of the mean. This is the standard deviation
--   divided by the square root of the sample size.
-- stdErrMeanOf :: (MonadThrow m) => v Double -> m Double
stdErrMeanOf
  :: (Real a, MonadThrow m)
  => Getting (Endo (Endo MeanKBN)) s a -> s -> m Double
{-# INLINE stdErrMeanOf #-}
stdErrMeanOf l xs = do
  est <- meanEstOf l xs
  return $! getMean est / (sqrt . fromIntegral . calcCount) est


-- -- -- | Weighted variance. This is biased estimation.
-- -- varianceWeighted :: (G.Vector v (Double,Double)) => v (Double,Double) -> Double
-- -- varianceWeighted samp
-- --     | G.length samp > 1 = fini $ robustSumVarWeighted samp
-- --     | otherwise         = 0
-- --     where
-- --       fini (V s w) = s / w
-- -- {-# SPECIALIZE varianceWeighted :: U.Vector (Double,Double) -> Double #-}
-- -- {-# SPECIALIZE varianceWeighted :: V.Vector (Double,Double) -> Double #-}


-- | Covariance of sample of pairs. 
covarianceOf
  :: (MonadThrow m)
  => (forall r. Getting (Endo (Endo r)) s (Double,Double)) -> s -> m Double
{-# INLINE covarianceOf #-}
covarianceOf l xy
  | n <= 0    = modErr "covarianceOf" "Insufficient sample size"
  | otherwise = return $! sumCov / fromIntegral n
  where
    -- Compute mean of each sample separately
    Cov n sumX sumY = doubleMeanOf l xy
    muX             = Sum.kbn sumX / fromIntegral n
    muY             = Sum.kbn sumY / fromIntegral n
    -- Compute covariance
    sumCov = covarianceSumOf l muX muY xy

data Cov = Cov !Int {-# UNPACK #-} !Sum.KBNSum {-# UNPACK #-} !Sum.KBNSum

correlationOf
  :: (MonadThrow m)
  => (forall r. Getting (Endo (Endo r)) s (Double,Double)) -> s -> m Double
{-# INLINE correlationOf #-}
correlationOf l xy
  | n <= 1    = modErr "correlationOf" "Insufficient sample size"
  | varX == 0 = modErr "correlationOf" "Sample X has 0 variance"
  | varY == 0 = modErr "correlationOf" "Sample Y has 0 variance"
  | otherwise = return $! sumCov
                        / (fromIntegral n * sqrt (varX * varY))
  where
    -- Mean of each sample
    Cov n sumX sumY = doubleMeanOf l xy
    muX             = Sum.kbn sumX / fromIntegral n
    muY             = Sum.kbn sumY / fromIntegral n
    -- Unbiased variance for each sample
    V sX sY = foldlOf' l
      (\(V sx sy) (x,y) -> V (addValue sx (square $ x - muX))
                             (addValue sy (square $ y - muY)))
      (V mempty mempty)
      xy
    varX = Sum.kbn sX / fromIntegral (n - 1)
    varY = Sum.kbn sY / fromIntegral (n - 1)
    -- Compute correlation
    sumCov = covarianceSumOf l muX muY xy

-- Compute mean of each sample
doubleMeanOf :: (forall r. Getting (Endo (Endo r)) s (Double,Double)) -> s -> Cov
{-# INLINE doubleMeanOf #-}
doubleMeanOf l = foldlOf' l
  (\(Cov i sx sy) (x,y) -> Cov (i+1) (addValue sx x) (addValue sy y))
  (Cov 0 mempty mempty)

covarianceSumOf
  :: (forall r. Getting (Endo (Endo r)) s (Double,Double))
  -> Double -> Double -> s -> Double
{-# INLINE covarianceSumOf #-}
covarianceSumOf l muX muY
  = stableSumOf (l . to (\(x,y) -> (x-muX)*(y-muY)))

-- ------------------------------------------------------------------------
-- -- Helper code. Monomorphic unpacked accumulators.

-- (^) operator from Prelude is just slow.
(^) :: Double -> Int -> Double
x ^ 1 = x
x ^ n = x * (x ^ (n-1))
{-# INLINE (^) #-}


liftErr :: MonadThrow m => String -> String -> Maybe a -> m a
liftErr f err = maybe (modErr f err) return
{-# INLINE liftErr #-}

modErr :: MonadThrow m => String -> String -> m a
modErr f err = throwM $ InvalidSample ("Statistics.Sample." ++ f) err

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
