{-# LANGUAGE TypeFamilies #-}
-- |
-- More generic imlementation of functions from 'Statistics.Sample'
module Statistics.Sample.New (
  -- * Statistics of location
    mean
  , meanWeighted
  ) where

import Statistics.Sample.Classes
import Statistics.Sample.Estimators

-- | /O(n)/ Arithmetic mean.  This uses Welford's algorithm to provide
-- numerical stability, using a single pass over the sample data.
mean :: (Elem s ~ Double, Sample s) => s -> Double
mean = calcMean . evalStatistics
{-# INLINE mean #-}

-- | /O(n)/ Arithmetic mean for weighted sample. It uses a single-pass
-- algorithm analogous to the one used by 'mean'.
meanWeighted :: (Elem s ~ (Double,Double), Sample s) => s -> Double
meanWeighted = calcMean . evalStatistics
{-# INLINE meanWeighted #-}
