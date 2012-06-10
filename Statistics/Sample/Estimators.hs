{-# LANGUAGE FlexibleContexts #-}
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
  , calcMeanWeight
    -- ** Robust variance
  , Variance
  , calcVariance
    -- ** Fast variance
  , FastVar
  , calcFastVar
  , calcFastVarUnb
  , calcFastStdDev
  , calcFastStdDevUnb
  , calcMeanFVar
  , calcCountFVar
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

instance FoldEstimator Count a where
  addElement (Count x) _ = Count (x+1)
  {-# INLINE addElement #-}

instance NullEstimator Count where
  nullEstimator   = Count 0
  {-# INLINE nullEstimator #-}

instance SemigoupEst Count where
  joinSample (Count n) (Count m) = Count (n+m)
  {-# INLINE joinSample #-}



----------------------------------------------------------------

newtype Sum a = Sum { calcSum :: a}
                deriving (Show,Eq,Typeable)

instance Num a => FoldEstimator (Sum a) a where
  addElement (Sum s) x = Sum (s + x)
  {-# INLINE addElement #-}

instance Num a => NullEstimator (Sum a)   where { nullEstimator   = Sum 0; {-# INLINE nullEstimator #-} }

instance Num a => SemigoupEst (Sum a) where
  joinSample (Sum a) (Sum b) = Sum (a + b)
  {-# INLINE joinSample #-}



----------------------------------------------------------------

-- | Find minimal element in the sample
newtype Min a = Min { calcMin :: a }
                deriving (Show,Eq,Typeable)

instance Ord a => NonEmptyEst (Min a) a where
  nonemptyEst = Init $ Est . Min
  {-# INLINE nonemptyEst #-}

instance Ord a => FoldEstimator (Min a) a where
  addElement (Min a) b = Min $ min a b
  {-# INLINE addElement #-}

instance Ord a => SemigoupEst (Min a) where
  joinSample (Min a) (Min b) = Min (min a b)
  {-# INLINE joinSample #-}



----------------------------------------------------------------

-- | Find maximal element in the sample
newtype Max a = Max { calcMax :: a }
                deriving (Show,Eq,Typeable)


instance Ord a => NonEmptyEst (Max a) a where
  nonemptyEst = Init $ Est . Max
  {-# INLINE nonemptyEst #-}

instance Ord a => FoldEstimator (Max a) a where
  addElement (Max a) b = Max $ max a b
  {-# INLINE addElement #-}

instance Ord a => SemigoupEst (Max a) where
  joinSample (Max a) (Max b) = Max (max a b)
  {-# INLINE joinSample #-}



----------------------------------------------------------------

-- | Accumulator for sample mean. It uses Welford's algorithm to
--   provide numerical stability
--
--   For elements of type 'Double' it calculates mean and for elements
--   of type '(Double,Double)' it calculates weighted mean. Weight is
--   second element of the tuple.
data Mean = Mean {
    calcMean       :: {-# UNPACK #-} !Double
    -- ^ Get mean estimate from accumulator.
  , calcMeanWeight :: {-# UNPACK #-} !Double
    -- ^ Get summary weight of elements. For unweighted mean it equal
    --   to total number of elements.
  }
  deriving (Eq,Show,Typeable)

instance FoldEstimator Mean Double where
  addElement (Mean m n) x = Mean m' n'
    where
      m' = m + (x - m) / n'
      n' = n + 1
  {-# INLINE addElement #-}
instance FoldEstimator Mean (Double,Double) where
  addElement e@(Mean m n) (x, w)
    | w == 0    = e
    | otherwise = Mean m' n'
    where
      n' = n + w
      m' = m + w * (x - m) / n'
  {-# INLINE addElement #-}

instance NullEstimator Mean where
  nullEstimator = Mean 0 0
  {-# INLINE nullEstimator #-}

instance SemigoupEst Mean where
   joinSample a@(Mean x n) b@(Mean y k)
     | n == 0    = a
     | k == 0    = b
     | otherwise = Mean ((x*n + y*k) / s) s
     where s = n + k
   {-# INLINE joinSample #-}




----------------------------------------------------------------

newtype Variance = Variance (Double -> Mean)

calcVariance :: Double
             -> Variance
             -> Double
calcVariance m (Variance s) = calcMean $ s m

instance FoldEstimator Variance Double where
  addElement (Variance f) x = Variance $ \m -> addElement (f m) (let d = x - m in d*d)
  {-# INLINE addElement #-}

instance NullEstimator Variance where
  nullEstimator = Variance $ \_ -> nullEstimator

instance SemigoupEst Variance where
  joinSample (Variance f) (Variance g) = Variance $ \m -> joinSample (f m) (g m)



----------------------------------------------------------------

data FastVar = FastVar
               {-# UNPACK #-} !Int
               {-# UNPACK #-} !Double
               {-# UNPACK #-} !Double
             deriving (Eq,Show,Typeable)

-- | Extract biased estimate of variance from accumulator
calcFastVar :: FastVar -> Double
calcFastVar (FastVar n _ s)
  | n > 1     = s / fromIntegral n
  | otherwise = 0

-- | Extract biased estimate of variance from accumulator
calcFastVarUnb :: FastVar -> Double
calcFastVarUnb (FastVar n _ s)
  | n > 1     = s / fromIntegral (n - 1)
  | otherwise = 0

-- | Extract biased estimate of variance from accumulator
calcFastStdDev :: FastVar -> Double
calcFastStdDev = sqrt . calcFastVar

-- | Extract biased estimate of variance from accumulator
calcFastStdDevUnb :: FastVar -> Double
calcFastStdDevUnb = sqrt . calcFastVarUnb

calcMeanFVar :: FastVar -> Double
calcMeanFVar (FastVar _ m _) = m

calcCountFVar :: FastVar -> Int
calcCountFVar (FastVar n _ _) = n

instance FoldEstimator FastVar Double where
  addElement (FastVar n m s) x = FastVar n' m' s'
    where
      n' = n + 1
      m' = m + d / fromIntegral n'
      s' = s + d * (x - m')
      d  = x - m
  {-# INLINE addElement #-}

instance NullEstimator FastVar where
  nullEstimator = FastVar 0 0 0
  {-# INLINE nullEstimator #-}



----------------------------------------------------------------

-- | Skip NaN in calculation of statistics
newtype SkipNaN m = SkipNaN { skipNaN :: m }
                    deriving (Show,Eq,Typeable)

instance FoldEstimator m a => FoldEstimator (SkipNaN m) a where
  addElement (SkipNaN m) x = SkipNaN (addElement m x)
  {-# INLINE addElement #-}

instance NullEstimator m => NullEstimator (SkipNaN m) where
  nullEstimator = SkipNaN nullEstimator
  {-# INLINE nullEstimator #-}

instance SemigoupEst m => SemigoupEst (SkipNaN m) where
  joinSample (SkipNaN m) (SkipNaN n) = SkipNaN (joinSample m n)
  {-# INLINE joinSample #-}
