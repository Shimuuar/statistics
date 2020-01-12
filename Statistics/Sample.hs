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

module Statistics.Sample
  where
    -- (
    -- -- * Types
    --   Sample
    -- , WeightedSample
    -- -- * Descriptive functions
    -- , stableSumOf
    -- , reduceSampleOf
    -- , minMaxOf
    -- , rangeOf

    -- -- * Statistics of location
    -- , mean
    -- , meanWeighted
    -- , harmonicMean
    -- , geometricMean

    -- -- * Statistics of dispersion
    -- -- $variance

    -- -- ** Two-pass functions (numerically robust)
    -- -- $robust
    -- , variance
    -- , varianceML
    -- , stdDev
    -- , stdDevML
    -- , stdErrMean

    -- -- ** Functions over central moments
    -- , centralMoment
    -- , centralMoments
    -- , skewness
    -- , kurtosis

    -- -- * Joint distirbutions
    -- , covariance
    -- , correlation
    -- , pair
    -- -- * References
    -- -- $references
    -- ) where

import Control.Applicative
import Control.Category    ((>>>))
import Control.Lens
import Control.Monad       (liftM)
import Control.Monad.Catch (MonadThrow(..))
import Statistics.Function (minMax,square)
import Statistics.Sample.Internal (robustSumVar, sum)
import Statistics.Types.Internal  (Sample,WeightedSample,StatisticsException(..))
import Statistics.Monoid.Class
import Statistics.Monoid.Numeric
import Data.Monoid         (Endo(..))
import qualified Data.Vector          as V
import qualified Data.Vector.Generic  as G
import qualified Data.Vector.Unboxed  as U
import qualified Data.Vector.Storable as S
import qualified Numeric.Sum as Sum
-- Operator ^ will be overridden
import Prelude hiding ((^), sum)


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



-- -- | Compute the skewness of a sample. This is a measure of the
-- -- asymmetry of its distribution.
-- --
-- -- A sample with negative skew is said to be /left-skewed/.  Most of
-- -- its mass is on the right of the distribution, with the tail on the
-- -- left.
-- --
-- -- > skewness $ U.to [1,100,101,102,103]
-- -- > ==> -1.497681449918257
-- --
-- -- A sample with positive skew is said to be /right-skewed/.
-- --
-- -- > skewness $ U.to [1,2,3,4,100]
-- -- > ==> 1.4975367033335198
-- --
-- -- A sample's skewness is not defined if its 'variance' is zero.
-- --
-- -- This function performs two passes over the sample, so is not subject
-- -- to stream fusion.
-- --
-- -- For samples containing many values very close to the mean, this
-- -- function is subject to inaccuracy due to catastrophic cancellation.
-- skewness :: (G.Vector v Double, MonadThrow m) => v Double -> m Double
-- skewness xs = do
--   (c3 , c2) <- centralMoments 3 2 xs
--   return $! c3 * c2 ** (-1.5)
-- {-# SPECIALIZE skewness :: MonadThrow m => V.Vector Double -> m Double #-}
-- {-# SPECIALIZE skewness :: MonadThrow m => U.Vector Double -> m Double #-}
-- {-# SPECIALIZE skewness :: MonadThrow m => S.Vector Double -> m Double #-}


-- -- | Compute the excess kurtosis of a sample.  This is a measure of
-- -- the \"peakedness\" of its distribution.  A high kurtosis indicates
-- -- that more of the sample's variance is due to infrequent severe
-- -- deviations, rather than more frequent modest deviations.
-- --
-- -- A sample's excess kurtosis is not defined if its 'variance' is
-- -- zero.
-- --
-- -- This function performs two passes over the sample, so is not subject
-- -- to stream fusion.
-- --
-- -- For samples containing many values very close to the mean, this
-- -- function is subject to inaccuracy due to catastrophic cancellation.
-- kurtosis :: (G.Vector v Double, MonadThrow m) => v Double -> m Double
-- kurtosis xs = do
--   (c4 , c2) <- centralMoments 4 2 xs
--   return $! c4 / (c2 * c2) - 3
-- {-# SPECIALIZE kurtosis :: MonadThrow m => V.Vector Double -> m Double #-}
-- {-# SPECIALIZE kurtosis :: MonadThrow m => U.Vector Double -> m Double #-}
-- {-# SPECIALIZE kurtosis :: MonadThrow m => S.Vector Double -> m Double #-}

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

-- | Unbiased estimate of a sample's variance. Also known as the
--   sample variance
--
--   \[ \sigma^2 = \frac{1}{N-1}\sum_{i=1}^N(x_i - \bar{x})^2 \]
varianceOf
  :: (MonadThrow m)
  => (forall r. Getting (Endo (Endo r)) s Double) -> s -> m Double
{-# INLINE varianceOf #-}
varianceOf l xs
  | n > 2
  , Just m <- calcMean accMean
    = return $! stableSumOf (l . to (subtract m >>> square)) xs
              / fromIntegral (n - 1)
  | otherwise = modErr "varianceOf" "Insufficient sample size"
  where
    accMean = asMeanKBN $ reduceSampleOf l xs
    n       = calcCount accMean


-- | Maximum likelihood estimate of a sample's variance. Also known
--   as the population variance:
--
--   \[ \sigma^2 = \frac{1}{N}\sum_{i=1}^N(x_i - \bar{x})^2 \]
varianceMLOf
  :: (MonadThrow m)
  => (forall r. Getting (Endo (Endo r)) s Double) -> s -> m Double
{-# INLINE varianceMLOf #-}
varianceMLOf l xs
  | n > 1
  , Just m <- calcMean accMean
    = return $! stableSumOf (l . to (subtract m >>> square)) xs
              / fromIntegral n
  | otherwise = modErr "varianceOf" "Insufficient sample size"
  where
    accMean = asMeanKBN $ reduceSampleOf l xs
    n       = calcCount accMean

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

-- -- | Standard error of the mean. This is the standard deviation
-- --   divided by the square root of the sample size.
-- stdErrMean :: (G.Vector v Double, MonadThrow m) => v Double -> m Double
-- stdErrMean xs = do
--   s <- stdDev xs
--   return $! s / (sqrt . fromIntegral . G.length) xs
-- {-# SPECIALIZE stdErrMean :: MonadThrow m => V.Vector Double -> m Double #-}
-- {-# SPECIALIZE stdErrMean :: MonadThrow m => U.Vector Double -> m Double #-}
-- {-# SPECIALIZE stdErrMean :: MonadThrow m => S.Vector Double -> m Double #-}


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


-- -- data SampleVariance = SampleVariance
-- --   { sampleSize  :: !Int
-- --   , sampleMean  :: !Double
-- --   , sampleSumSq :: !Double
-- --   }

-- -- meanVariance :: (G.Vector v Double) => v Double -> SampleVariance
-- -- meanVariance xs = SampleVariance
-- --   { sampleSize  = n
-- --   , sampleMean  = m
-- --   , sampleSumSq = robustSumVar m xs
-- --   }
-- --   where
-- --     n             = G.length xs
-- --     m | n == 0    = 0
-- --       | otherwise = sum xs / fromIntegral n
-- -- {-# SPECIALIZE meanVariance :: V.Vector Double -> SampleVariance #-}
-- -- {-# SPECIALIZE meanVariance :: U.Vector Double -> SampleVariance #-}
-- -- {-# SPECIALIZE meanVariance :: S.Vector Double -> SampleVariance #-}

-- -- -- | Calculate mean and maximum likelihood estimate of variance. This
-- -- -- function should be used if both mean and variance are required
-- -- -- since it will calculate mean only once.
-- -- meanVariance ::  (G.Vector v Double) => v Double -> (Double,Double)
-- -- meanVariance samp
-- --   | n > 1     = (m, robustSumVar m samp / fromIntegral n)
-- --   | otherwise = (m, 0)
-- --     where
-- --       n = G.length samp
-- --       m = mean samp
-- -- {-# SPECIALIZE meanVariance :: U.Vector Double -> (Double,Double) #-}
-- -- {-# SPECIALIZE meanVariance :: V.Vector Double -> (Double,Double) #-}

-- -- -- | Calculate mean and unbiased estimate of variance. This
-- -- -- function should be used if both mean and variance are required
-- -- -- since it will calculate mean only once.
-- -- meanVarianceUnb :: (G.Vector v Double) => v Double -> (Double,Double)
-- -- meanVarianceUnb samp
-- --   | n > 1     = (m, robustSumVar m samp / fromIntegral (n-1))
-- --   | otherwise = (m, 0)
-- --     where
-- --       n = G.length samp
-- --       m = mean samp
-- -- {-# SPECIALIZE meanVarianceUnb :: U.Vector Double -> (Double,Double) #-}
-- -- {-# SPECIALIZE meanVarianceUnb :: V.Vector Double -> (Double,Double) #-}

-- -- -- | Standard deviation.  This is simply the square root of the
-- -- -- unbiased estimate of the variance.
-- -- stdDev :: (G.Vector v Double) => v Double -> Double
-- -- stdDev = sqrt . varianceUnbiased
-- -- {-# SPECIALIZE stdDev :: U.Vector Double -> Double #-}
-- -- {-# SPECIALIZE stdDev :: V.Vector Double -> Double #-}

-- -- -- | Standard error of the mean. This is the standard deviation
-- -- -- divided by the square root of the sample size.
-- -- stdErrMean :: (G.Vector v Double) => v Double -> Double
-- -- stdErrMean samp = stdDev samp / (sqrt . fromIntegral . G.length) samp
-- -- {-# SPECIALIZE stdErrMean :: U.Vector Double -> Double #-}
-- -- {-# SPECIALIZE stdErrMean :: V.Vector Double -> Double #-}

-- -- robustSumVarWeighted :: (G.Vector v (Double,Double)) => v (Double,Double) -> V
-- -- robustSumVarWeighted samp = G.foldl' go (V 0 0) samp
-- --     where
-- --       go (V s w) (x,xw) = V (s + xw*d*d) (w + xw)
-- --           where d = x - m
-- --       m = meanWeighted samp
-- -- {-# INLINE robustSumVarWeighted #-}

-- -- -- | Weighted variance. This is biased estimation.
-- -- varianceWeighted :: (G.Vector v (Double,Double)) => v (Double,Double) -> Double
-- -- varianceWeighted samp
-- --     | G.length samp > 1 = fini $ robustSumVarWeighted samp
-- --     | otherwise         = 0
-- --     where
-- --       fini (V s w) = s / w
-- -- {-# SPECIALIZE varianceWeighted :: U.Vector (Double,Double) -> Double #-}
-- -- {-# SPECIALIZE varianceWeighted :: V.Vector (Double,Double) -> Double #-}


-- -- | Covariance of sample of pairs. For empty sample it's set to
-- --   zero
-- covariance :: (G.Vector v (Double,Double), G.Vector v Double, MonadThrow m)
--            => v (Double,Double)
--            -> m Double
-- covariance xy
--   | n <= 0    = modErr "covariance" "Insufficient sample size"
--   | otherwise = do
--       muX <- mean xs
--       muY <- mean ys
--       mean $ G.zipWith (*)
--         (G.map (\x -> x - muX) xs)
--         (G.map (\y -> y - muY) ys)
--    where
--      n       = G.length xy
--      (xs,ys) = G.unzip xy
-- {-# SPECIALIZE covariance :: MonadThrow m => U.Vector (Double,Double) -> m Double #-}
-- {-# SPECIALIZE covariance :: MonadThrow m => V.Vector (Double,Double) -> m Double #-}

-- -- | Correlation coefficient for sample of pairs. Also known as
-- --   Pearson's correlation. For empty sample it's set to zero.
-- correlation :: (G.Vector v (Double,Double), G.Vector v Double, MonadThrow m)
--            => v (Double,Double)
--            -> m Double
-- correlation xy
--   | n <= 1    = modErr "correlation" "Insufficient sample size"
--   | otherwise = do
--       cov <- mean $ G.zipWith (*)
--         (G.map (\x -> x - muX) xs)
--         (G.map (\y -> y - muY) ys)
--       return $! cov / sqrt (varX * varY)
--   where
--     n       = G.length xy
--     (xs,ys) = G.unzip  xy
--     (muX,varX) = undefined -- meanVariance xs
--     (muY,varY) = undefined -- meanVariance ys
-- {-# SPECIALIZE correlation :: MonadThrow m => U.Vector (Double,Double) -> m Double #-}
-- {-# SPECIALIZE correlation :: MonadThrow m => V.Vector (Double,Double) -> m Double #-}

-- -- | Pair two samples. It's like 'G.zip' but requires that both
-- --   samples have equal size.
-- pair :: (G.Vector v a, G.Vector v b, G.Vector v (a,b), MonadThrow m)
--      => v a -> v b -> m (v (a,b))
-- pair va vb
--   | G.length va == G.length vb = return $ G.zip va vb
--   | otherwise                  = modErr "pair" "vectors must have same length"
-- {-# INLINE pair #-}

-- ------------------------------------------------------------------------
-- -- Helper code. Monomorphic unpacked accumulators.

-- (^) operator from Prelude is just slow.
(^) :: Double -> Int -> Double
x ^ 1 = x
x ^ n = x * (x ^ (n-1))
{-# INLINE (^) #-}

-- -- don't support polymorphism, as we can't get unboxed returns if we use it.
-- data T = T {-# UNPACK #-}!Double {-# UNPACK #-}!Int

-- data T1 = T1 {-# UNPACK #-}!Int {-# UNPACK #-}!Double {-# UNPACK #-}!Double

-- {-

-- Consider this core:

-- with data T a = T !a !Int

-- $wfold :: Double#
--                -> Int#
--                -> Int#
--                -> (# Double, Int# #)

-- and without,

-- $wfold :: Double#
--                -> Int#
--                -> Int#
--                -> (# Double#, Int# #)

-- yielding to boxed returns and heap checks.

-- -}

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
