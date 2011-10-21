{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
module Statistics.Sample.Estimators (
    -- * Sample estimators
    -- ** Count nume
    Count
  , calcCount
    -- ** Sum of all elements
  , Sum
  , calcSum
    -- ** Maximum and minimum
  , Min
  , calcMin
  , Max
  , calcMax
    -- ** Sample mean
  , Mean
  , calcMean
  , calcCountMean
    -- * Transformers
  , SkipNaN
  , skipNaN
  ) where

import Data.Typeable (Typeable)

import Statistics.Sample.Classes

----------------------------------------------------------------

-- | Count number of elements in the sample
newtype Count = Count { 
  calcCount :: Int 
  }
  deriving (Eq,Show,Typeable)

instance FoldEstimator Count where
  type StandardElem Count = ()
  addStdElement (Count x) _ = Count (x+1)
  {-# INLINE addStdElement #-}

instance SingletonEst  Count where { singletonStat _ = Count 1; {-# INLINE singletonStat #-} }
instance NullEstimator Count where { nullEstimator   = Count 0; {-# INLINE nullEstimator #-} }

instance SemigoupEst Count where
  joinSample (Count n) (Count m) = Count (n+m)
  {-# INLINE joinSample #-}

instance Accept Count a where
  transformElem _ _ = ()
  {-# INLINE transformElem #-}



----------------------------------------------------------------

newtype Sum a = Sum { calcSum :: a} 
                deriving (Show,Eq,Typeable)

instance Num a => FoldEstimator (Sum a) where
  type StandardElem (Sum a) = a
  addStdElement (Sum s) x = Sum (s + x)
  {-# INLINE addStdElement #-}

instance Num a => SingletonEst  (Sum a) where { singletonStat x = Sum x; {-# INLINE singletonStat #-} }
instance Num a => NullEstimator (Sum a) where { nullEstimator   = Sum 0; {-# INLINE nullEstimator #-} }

instance Num a => SemigoupEst (Sum a) where
  joinSample (Sum a) (Sum b) = Sum (a + b)
  {-# INLINE joinSample #-}

instance Num a => Accept (Sum a) a where
  transformElem _ = id
  {-# INLINE transformElem #-}

----------------------------------------------------------------

-- | Find minimal element in the sample
newtype Min a = Min { calcMin :: a }
                deriving (Show,Eq,Typeable)

instance Ord a => FoldEstimator (Min a) where
  type StandardElem (Min a) = a
  addStdElement (Min a) b = Min $ min a b
  {-# INLINE addStdElement #-}

instance Ord a => SingletonEst (Min a) where
  singletonStat = Min
  {-# INLINE singletonStat #-}

instance Ord a => SemigoupEst (Min a) where
  joinSample (Min a) (Min b) = Min (min a b)
  {-# INLINE joinSample #-}

instance Ord a => Accept (Min a) a where
  transformElem _ = id
  {-# INLINE transformElem #-}

----------------------------------------------------------------

-- | Find maximal element in the sample
newtype Max a = Max { calcMax :: a }
                deriving (Show,Eq,Typeable)

instance Ord a => FoldEstimator (Max a) where
  type StandardElem (Max a) = a
  addStdElement (Max a) b = Max $ max a b
  {-# INLINE addStdElement #-}

instance Ord a => SingletonEst (Max a) where
  singletonStat = Max
  {-# INLINE singletonStat #-}

instance Ord a => SemigoupEst (Max a) where
  joinSample (Max a) (Max b) = Max (max a b)
  {-# INLINE joinSample #-}

instance Ord a => Accept (Max a) a where
  transformElem _ = id
  {-# INLINE transformElem #-}

----------------------------------------------------------------

data Mean = Mean {
    calcMean      :: Double 
  , calcCountMean :: Int
  }
  deriving (Eq,Show,Typeable)

instance FoldEstimator Mean where
  type StandardElem Mean = Double
  addStdElement (Mean m n) x = Mean m' n'
    where
      m' = m + (x - m) / fromIntegral n'
      n' = n + 1
  {-# INLINE addStdElement #-}

instance SingletonEst  Mean where { singletonStat x = Mean x 1; {-# INLINE singletonStat #-} }
instance NullEstimator Mean where { nullEstimator   = Mean 0 0; {-# INLINE nullEstimator #-} }

instance SemigoupEst Mean where
   joinSample !(Mean x n) !(Mean y k) =
     Mean ((x*n' + y*k') / (n' + k')) (n + k)
     where
       n' = fromIntegral n
       k' = fromIntegral k
   {-# INLINE joinSample #-}



----------------------------------------------------------------

-- | Skip NaN in calculation of statistics
newtype SkipNaN m = SkipNaN { skipNaN :: m }
                    deriving (Show,Eq,Typeable)

instance FoldEstimator m => FoldEstimator (SkipNaN m) where
  type StandardElem (SkipNaN m) = StandardElem m
  addStdElement (SkipNaN m) x = SkipNaN (addStdElement m x)
  {-# INLINE addStdElement #-}

instance SingletonEst m => SingletonEst (SkipNaN m) where
  singletonStat x = SkipNaN (singletonStat x)
  {-# INLINE singletonStat #-}
instance NullEstimator m => NullEstimator (SkipNaN m) where
  nullEstimator = SkipNaN nullEstimator
  {-# INLINE nullEstimator #-}

instance SemigoupEst m => SemigoupEst (SkipNaN m) where
  joinSample (SkipNaN m) (SkipNaN n) = SkipNaN (joinSample m n)
  {-# INLINE joinSample #-}