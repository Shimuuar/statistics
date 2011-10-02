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
  ) where

import Data.List

----------------------------------------------------------------
-- Samples
----------------------------------------------------------------

-- | Type class for samples.
class Sample a where
  type Elem a :: *
  -- | Strict left fold over sample
  foldSample    :: (acc -> Elem a -> acc) -> acc -> a -> acc
  -- | Fold combined with map
  mapFoldSample :: (Elem a -> b) -> (acc -> b -> acc) -> acc -> a -> acc

instance Sample [a] where
  type Elem [a] = a
  foldSample            = foldl'
  mapFoldSample f acc x = foldl' acc x . map f

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
class FoldStatistic m a where
  pappend :: m -> a -> m

-- | Type class for
class FoldStatistic m a => SingletonStat m a where
  singletonStat :: a -> m

-- | Statistic accumulators which are meaningful for
class NullStatistic m where
  emptySample :: m

-- | Statistic accumulators which admit efficient join.
class SemigoupStat m where
  joinSample :: m -> m -> m
