{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Statistics.Sample.Accumulators (
    -- * Generic API and type classes
    calculate
    -- * Accumulators
  , Count(..)
  , Binomial(..)
  , WelfordMean(..)
    -- ** Summation
  ) where

import Data.Monoid
import Data.Folds
import Numeric.Sum hiding (sum)


----------------------------------------------------------------
-- Generic API and type classes
----------------------------------------------------------------

-- | Calculate statistics from sample
calculate :: (Sample s, Accumulator m (Element s)) => (m -> a) -> s -> a
calculate f src = f $ runFold (fromAcc +<< toSource src)
{-# INLINE calculate #-}



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

instance Fractional a => Accumulator (WelfordMean a) a where
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

