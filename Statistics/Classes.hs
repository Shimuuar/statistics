{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Statistics.Classes (
    -- * Type class for data samples
    Sample(..)
    -- * Type class for
    -- $classes
  , FoldStatistic(..)
  , SingletonStat(..)
  , NullStatistic(..)
  , SemigoupStat(..)
    -- * Data types
  , Mean
  ) where

import Data.List

----------------------------------------------------------------
-- Samples
----------------------------------------------------------------

-- | Type class for samples.
class Sample a where
  -- | Element type of a sample
  type Elem a :: *
  -- | Strict left fold over sample
  foldSample    :: (acc -> Elem a -> acc) -> acc -> a -> acc

instance Sample [a] where
  type Elem [a] = a
  foldSample    = foldl'
  {-# INLINE foldSample #-}

----------------------------------------------------------------
-- Accumulators
----------------------------------------------------------------

-- $classes
--
-- FIXME: no documentation
--
-- These classes comes with rather large set of laws which tie
-- together unrelated modules.


-- | Type class for statistics which could be expresed by fold.
class FoldStatistic m where
  -- | Standard elemnt of sample
  type SampleElem m :: *
  pappend :: m -> SampleElem m -> m

-- | Type class for
class FoldStatistic m => SingletonStat m where
  singletonStat :: SampleElem m -> m

-- | Statistic accumulators which are meaningful for
class SingletonStat m => NullStatistic m where
  emptySample :: m

-- | Statistic accumulators which admit efficient join.
class FoldStatistic m => SemigoupStat m where
  joinSample :: m -> m -> m



----------------------------------------------------------------

data Mean = Mean Double Int
            deriving (Eq,Show)

instance FoldStatistic Mean where
  type SampleElem Mean = Double
  pappend (Mean m n) x = Mean m' n'
    where
      m' = m + (x - m) / fromIntegral n'
      n' = n + 1

instance SingletonStat Mean where
  singletonStat x = Mean x 1

instance NullStatistic Mean where
  emptySample = Mean 0 0

instance SemigoupStat Mean where
   joinSample !(Mean x n) !(Mean y k) =
     Mean ((x*n' + y*k') / (n' + k')) (n + k)
     where
       n' = fromIntegral n
       k' = fromIntegral k
   {-# INLINE joinSample #-}
