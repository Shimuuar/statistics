{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE ViewPatterns       #-}
-- |
-- Module    : Statistics.Quantile
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Functions for approximating quantiles, i.e. points taken at regular
-- intervals from the cumulative distribution function of a random
-- variable.
--
-- The number of quantiles is described below by the variable /q/, so
-- with /q/=4, a 4-quantile (also known as a /quartile/) has 4
-- intervals, and contains 5 points.  The parameter /k/ describes the
-- desired point, where 0 ≤ /k/ ≤ /q/.
module Statistics.Quantile
    (
    -- * Quantile estimation functions
    -- $cont_quantiles
      ContParam(..)
    , Default(..)
    , quantileOf
    , quantilesOf
    , quantilesVecOf
    -- ** Parameters for the continuous sample method
    , cadpw
    , hazen
    , spss
    , s
    , medianUnbiased
    , normalUnbiased
    -- * Other algorithms
    , weightedAvg
    -- * Median & other specializations
    , medianOf
    , madOf
    , midspreadOf
    -- * References
    -- $references
    ) where

import           Control.Lens
import           Control.Monad.Catch    (MonadThrow(..))
import           Data.Binary            (Binary)
import           Data.Aeson             (ToJSON,FromJSON)
import           Data.Data              (Data,Typeable)
import           Data.Default.Class
import           Data.Monoid            (Endo)
import           Data.Functor
import qualified Data.Foldable        as F
import           Data.Vector.Generic ((!))
import qualified Data.Vector          as V
import qualified Data.Vector.Generic  as G
import qualified Data.Vector.Unboxed  as U
import qualified Data.Vector.Storable as S
import GHC.Generics (Generic)

import Statistics.Function (partialSort,toVectorOf)
import Statistics.Types    (StatisticsException(..))

----------------------------------------------------------------
-- Quantile estimation
----------------------------------------------------------------

-- | O(/n/·log /n/). Estimate the /k/th /q/-quantile of a sample,
-- using the weighted average method. Up to rounding errors it's same
-- as @quantile s@.
--
-- The following properties should hold otherwise an error will be thrown.
--
--   * the length of the input is greater than @0@
--
--   * the input does not contain @NaN@
--
--   * k ≥ 0 and k ≤ q
weightedAvg :: (G.Vector v Double, MonadThrow m)
            => Int        -- ^ /k/, the desired quantile.
            -> Int        -- ^ /q/, the number of quantiles.
            -> v Double   -- ^ /x/, the sample data.
            -> m Double
weightedAvg k q x
  | G.any isNaN x   = modErr "weightedAvg" "Sample contains NaNs"
  | n == 0          = modErr "weightedAvg" "Sample is empty"
  | n == 1          = return $! G.head x
  | q < 2           = modErr "weightedAvg" "At least 2 quantiles is needed"
  | k == q          = return $! G.maximum x
  | k >= 0 || k < q = return $! xj + g * (xj1 - xj)
  | otherwise       = modErr "weightedAvg" "Wrong quantile number"
  where
    j   = floor idx
    idx = fromIntegral (n - 1) * fromIntegral k / fromIntegral q
    g   = idx - fromIntegral j
    xj  = sx ! j
    xj1 = sx ! (j+1)
    sx  = partialSort (j+2) x
    n   = G.length x
{-# SPECIALIZE weightedAvg :: MonadThrow m => Int -> Int -> V.Vector Double -> m Double #-}
{-# SPECIALIZE weightedAvg :: MonadThrow m => Int -> Int -> U.Vector Double -> m Double #-}
{-# SPECIALIZE weightedAvg :: MonadThrow m => Int -> Int -> S.Vector Double -> m Double #-}


----------------------------------------------------------------
-- Quantiles continuous algorithm
----------------------------------------------------------------

-- $cont_quantiles
--
-- Below is family of functions which use same algorithm for estimation
-- of sample quantiles. It approximates empirical CDF as continuous
-- piecewise function which interpolates linearly between points
-- \((X_k,p_k)\) where \(X_k\) is k-th order statistics (k-th smallest
-- element) and \(p_k\) is probability corresponding to
-- it. 'ContParam' determines how \(p_k\) is chosen. For more detailed
-- explanation see [Hyndman1996].
--
-- This is the method used by most statistical software, such as R,
-- Mathematica, SPSS, and S.


-- | Parameters /α/ and /β/ to the 'continuousBy' function. Exact
--   meaning of parameters is described in [Hyndman1996] in section
--   \"Piecewise linear functions\"
data ContParam = ContParam {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  deriving (Show,Eq,Ord,Data,Typeable,Generic)

-- | We use 's' as default value which is same as R's default.
instance Default ContParam where
  def = s

instance Binary   ContParam
instance ToJSON   ContParam
instance FromJSON ContParam

-- | O(/n/·log /n/). Estimate the /k/th /q/-quantile of a sample /x/,
--   using the continuous sample method with the given parameters.
--
--   The following properties should hold, otherwise an error will be thrown.
--
--     * input sample must be nonempty
--
--     * the input does not contain @NaN@
--
--     * 0 ≤ k ≤ q
quantileOf
  :: (MonadThrow m)
  => Getting (Endo [Double]) s Double
  -> ContParam  -- ^ Parameters /α/ and /β/.
  -> Int        -- ^ /k/, the desired quantile.
  -> Int        -- ^ /q/, the number of quantiles.
  -> s          -- ^ /x/, the sample data.
  -> m Double
{-# INLINE quantileOf #-}
quantileOf l param q nQ
  = quantileVector param q nQ
  . toVectorOf l

-- | O(/k·n/·log /n/). Estimate set of the /k/th /q/-quantile of a
--   sample /x/, using the continuous sample method with the given
--   parameters. This is faster than calling quantile repeatedly since
--   sample should be sorted only once
--
--   The following properties should hold, otherwise an error will be thrown.
--
--     * input sample must be nonempty
--
--     * the input does not contain @NaN@
--
--     * for every k in set of quantiles 0 ≤ k ≤ q
quantilesOf
  :: (F.Foldable f, Functor f, MonadThrow m)
  => Getting (Endo [Double]) s Double
  -> ContParam
  -> f Int
  -> Int
  -> s
  -> m (f Double)
{-# INLINE quantilesOf #-}
quantilesOf l param qs nQ
  = quantilesVector param qs nQ
  . toVectorOf l

-- | O(/k·n/·log /n/). Same as quantiles but uses 'G.Vector' container
--   instead of 'Foldable' one.
quantilesVecOf :: (G.Vector v Double, G.Vector v Int, MonadThrow m)
  => Getting (Endo [Double]) s Double
  -> ContParam
  -> v Int
  -> Int
  -> s
  -> m (v Double)
{-# INLINE quantilesVecOf #-}
quantilesVecOf l param qs nQ
  = quantilesVecVector param qs nQ
  . toVectorOf l


-- Returns True if quantile number is out of range
badQ :: Int -> Int -> Bool
badQ nQ q = q < 0 || q > nQ

-- Obtain k from equation for p_k [Hyndman1996] p.363.  Note that
-- equation defines p_k for integer k but we calculate it as real
-- value and will use fractional part for linear interpolation. This
-- is correct since equation is linear.
toPk
  :: ContParam
  -> Int        -- ^ /n/ number of elements
  -> Int        -- ^ /k/, the desired quantile.
  -> Int        -- ^ /q/, the number of quantiles.
  -> Double
toPk (ContParam a b) (fromIntegral -> n) q nQ
  = a + p * (n + 1 - a - b)
  where
    p = fromIntegral q / fromIntegral nQ

-- Estimate quantile for given k (including fractional part)
estimateQuantile :: G.Vector v Double => v Double -> Double -> Double
{-# INLINE estimateQuantile #-}
estimateQuantile sortedXs k'
  = (1-g) * item (k-1) + g * item k
  where
    (k,g) = properFraction k'
    item  = (sortedXs !) . clamp
    --
    clamp = max 0 . min (n - 1)
    n     = G.length sortedXs

psort :: G.Vector v Double => v Double -> Int -> v Double
psort xs k = partialSort (max 0 $ min (G.length xs - 1) k) xs
{-# INLINE psort #-}


-- | California Department of Public Works definition, /α/=0, /β/=1.
-- Gives a linear interpolation of the empirical CDF.  This
-- corresponds to method 4 in R and Mathematica.
cadpw :: ContParam
cadpw = ContParam 0 1

-- | Hazen's definition, /α/=0.5, /β/=0.5.  This is claimed to be
-- popular among hydrologists.  This corresponds to method 5 in R and
-- Mathematica.
hazen :: ContParam
hazen = ContParam 0.5 0.5

-- | Definition used by the SPSS statistics application, with /α/=0,
-- /β/=0 (also known as Weibull's definition).  This corresponds to
-- method 6 in R and Mathematica.
spss :: ContParam
spss = ContParam 0 0

-- | Definition used by the S statistics application, with /α/=1,
-- /β/=1.  The interpolation points divide the sample range into @n-1@
-- intervals.  This corresponds to method 7 in R and Mathematica and
-- is default in R.
s :: ContParam
s = ContParam 1 1

-- | Median unbiased definition, /α/=1\/3, /β/=1\/3. The resulting
-- quantile estimates are approximately median unbiased regardless of
-- the distribution of /x/.  This corresponds to method 8 in R and
-- Mathematica.
medianUnbiased :: ContParam
medianUnbiased = ContParam third third
    where third = 1/3

-- | Normal unbiased definition, /α/=3\/8, /β/=3\/8.  An approximately
-- unbiased estimate if the empirical distribution approximates the
-- normal distribution.  This corresponds to method 9 in R and
-- Mathematica.
normalUnbiased :: ContParam
normalUnbiased = ContParam ta ta
    where ta = 3/8

modErr :: MonadThrow m => String -> String -> m a
modErr f err = throwM $ InvalidSample ("Statistics.Quantile." ++ f) err


----------------------------------------------------------------
-- Specializations
----------------------------------------------------------------

-- | O(/n/·log /n/) Estimate median of sample
medianOf
  :: (MonadThrow m)
  => Getting (Endo [Double]) s Double
  -> ContParam  -- ^ Parameters /α/ and /β/.
  -> s          -- ^ /x/, the sample data.
  -> m Double
{-# INLINE medianOf #-}
medianOf l p = quantileOf l p 1 2

-- | O(/n/·log /n/). Estimate the range between /q/-quantiles 1 and
-- /q/-1 of a sample /x/, using the continuous sample method with the
-- given parameters.
--
-- For instance, the interquartile range (IQR) can be estimated as
-- follows:
--
-- > midspread medianUnbiased 4 (U.fromList [1,1,2,2,3])
-- > ==> 1.333333
midspreadOf
  :: (MonadThrow m)
  => Getting (Endo [Double]) s Double
  -> ContParam  -- ^ Parameters /α/ and /β/.
  -> Int        -- ^ /q/, the number of quantiles.
  -> s          -- ^ /x/, the sample data.
  -> m Double
{-# INLINE midspreadOf #-}
midspreadOf l param k x
  | k <= 0    = modErr "midspread" "Nonpositive number of quantiles"
  | otherwise = do Pair x1 x2 <- quantilesOf l param (Pair 1 (k-1)) k x
                   return $! x2 - x1

data Pair a = Pair !a !a
  deriving (Functor, F.Foldable)


-- | O(/n/·log /n/). Estimate the median absolute deviation (MAD) of a
--   sample /x/ using 'continuousBy'. It's robust estimate of
--   variability in sample and defined as:
--
--   \[
--   MAD = \operatorname{median}(| X_i - \operatorname{median}(X) |)
--   \]
madOf :: (MonadThrow m)
    => Getting (Endo [Double]) s Double
    -> ContParam  -- ^ Parameters /α/ and /β/.
    -> s          -- ^ /x/, the sample data.
    -> m Double
{-# INLINE madOf #-}
madOf l p xs = do
  med <- medianVec vec
  medianVec $ U.map (abs . subtract med) vec
  where
    -- We want to share temporary vector. Lens could possibly do quite
    -- a bit of work (filtering, for example)
    vec       = toVectorOf l xs
    medianVec = quantileVector p 1 2

----------------------------------------------------------------
-- Vector specializations
----------------------------------------------------------------

quantileVector
  :: (MonadThrow m)
  => ContParam       -- ^ Parameters /α/ and /β/.
  -> Int             -- ^ /k/, the desired quantile.
  -> Int             -- ^ /q/, the number of quantiles.
  -> U.Vector Double -- ^ /x/, the sample data.
  -> m Double
quantileVector param q nQ xs
  | nQ < 2         = modErr "quantile" "At least 2 quantiles is needed"
  | badQ nQ q      = modErr "quantile" "Wrong quantile number"
  | U.any isNaN xs = modErr "quantile" "Sample contains NaNs"
  | otherwise      = return $! estimateQuantile sortedXs pk
  where
    pk       = toPk param n q nQ
    sortedXs = psort xs $ floor pk + 1
    n        = G.length xs

quantilesVector
  :: (F.Foldable f, Functor f, MonadThrow m)
  => ContParam
  -> f Int
  -> Int
  -> U.Vector Double
  -> m (f Double)
quantilesVector param qs nQ xs
  | nQ < 2             = modErr "quantiles" "At least 2 quantiles is needed"
  | F.any (badQ nQ) qs = modErr "quantiles" "Wrong quantile number"
  | U.any isNaN xs     = modErr "quantiles" "Sample contains NaNs"
  -- Doesn't matter what we put into empty container
  | fnull qs           = return $! 0 <$ qs
  | otherwise          = return $! fmap (estimateQuantile sortedXs) ks'
  where
    ks'      = fmap (\q -> toPk param n q nQ) qs
    sortedXs = psort xs $ floor (F.maximum ks') + 1
    n        = G.length xs

quantilesVecVector
  :: (G.Vector v Int, G.Vector v Double, MonadThrow m)
  => ContParam
  -> v Int
  -> Int
  -> U.Vector Double
  -> m (v Double)
quantilesVecVector param qs nQ xs
  | nQ < 2             = modErr "quantiles" "At least 2 quantiles is needed"
  | G.any (badQ nQ) qs = modErr "quantiles" "Wrong quantile number"
  | G.any isNaN xs     = modErr "quantiles" "Sample contains NaNs"
  -- Doesn't matter what we put into empty container
  | G.null qs          = return G.empty
  | otherwise          = return $! G.map (estimateQuantile sortedXs) ks'
  where
    ks'      = G.map (\q -> toPk param n q nQ) qs
    sortedXs = psort xs $ floor (G.maximum ks') + 1
    n        = G.length xs

-- COMPAT
fnull :: F.Foldable f => f a -> Bool
#if !MIN_VERSION_base(4,8,0)
fnull = F.foldr (\_ _ -> False) True
#else
fnull = null
#endif


-- $references
--
-- * Weisstein, E.W. Quantile. /MathWorld/.
--   <http://mathworld.wolfram.com/Quantile.html>
--
-- * [Hyndman1996] Hyndman, R.J.; Fan, Y. (1996) Sample quantiles in statistical
--   packages. /American Statistician/
--   50(4):361&#8211;365. <http://www.jstor.org/stable/2684934>
