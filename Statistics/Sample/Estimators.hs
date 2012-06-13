{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
module Statistics.Sample.Estimators (
    -- * Statistics
    Count(..)
  , Min(..)
  , Max(..)
  , Mean(..)
  , Variance(..)
  , StdDev(..)
  , VarianceBiased(..)
  , StdDevBiased(..)
    -- * Estimators
  , CountEst(..)
  , MinEst(..)
  , MaxEst(..)
    -- ** Sample mean and variance
  , MeanEst(..)
  , VarianceEst(..)
  , FastVar(..)
    -- * Transformers
  , SkipNaN(..)
  ) where

import Data.Data (Typeable,Data)

import Statistics.Sample.Classes


----------------------------------------------------------------
-- Statistics
----------------------------------------------------------------

newtype Count = Count { calcCount :: Int }
                deriving (Eq,Show,Typeable,Data)

newtype Min a = Min { calcMin :: a }
                deriving (Eq,Show,Typeable,Data)

newtype Max a = Max { calcMax :: a }
                deriving (Eq,Show,Typeable,Data)

newtype Mean = Mean { calcMean :: Double }
               deriving (Eq,Show,Typeable,Data)

newtype Variance = Variance { calcVariance :: Double }
               deriving (Eq,Show,Typeable,Data)

newtype StdDev = StdDev { calcStdDev :: Double }
               deriving (Eq,Show,Typeable,Data)

newtype VarianceBiased = VarianceBiased { calcVarianceBiased :: Double }
                       deriving (Eq,Show,Typeable,Data)

newtype StdDevBiased = StdDevBiased { calcStdDevBiased :: Double }
                     deriving (Eq,Show,Typeable,Data)


----------------------------------------------------------------
-- Estimators
----------------------------------------------------------------


-- | Count number of elements in the sample
newtype CountEst = CountEst Int

instance FoldEstimator CountEst a where
  addElement (CountEst x) _ = CountEst (x+1)
  {-# INLINE addElement #-}

instance NullEstimator CountEst where
  nullEstimator   = CountEst 0
  {-# INLINE nullEstimator #-}

instance SemigoupEst CountEst where
  joinSample (CountEst n) (CountEst m) = CountEst (n+m)
  {-# INLINE joinSample #-}

instance Calc CountEst Count where
  calc (CountEst n) = Count n
  {-# INLINE calc #-}



----------------------------------------------------------------

-- | Find minimal element in the sample
newtype MinEst a = MinEst a
                deriving (Show,Eq,Typeable,Data)

instance Ord a => NonEmptyEst (MinEst a) a where
  nonemptyEst = Init $ Est . MinEst
  {-# INLINE nonemptyEst #-}

instance Ord a => FoldEstimator (MinEst a) a where
  addElement (MinEst a) b = MinEst $ min a b
  {-# INLINE addElement #-}

instance Ord a => SemigoupEst (MinEst a) where
  joinSample (MinEst a) (MinEst b) = MinEst (min a b)
  {-# INLINE joinSample #-}

instance Calc (MinEst a) (Min a) where
  calc (MinEst x) = Min x
  {-# INLINE calc #-}



----------------------------------------------------------------

-- | Find maximal element in the sample
newtype MaxEst a = MaxEst a
                deriving (Show,Eq,Typeable,Data)

instance Ord a => NonEmptyEst (MaxEst a) a where
  nonemptyEst = Init $ Est . MaxEst
  {-# INLINE nonemptyEst #-}

instance Ord a => FoldEstimator (MaxEst a) a where
  addElement (MaxEst a) b = MaxEst $ max a b
  {-# INLINE addElement #-}

instance Ord a => SemigoupEst (MaxEst a) where
  joinSample (MaxEst a) (MaxEst b) = MaxEst (max a b)
  {-# INLINE joinSample #-}

instance Calc (MaxEst a) (Max a) where
  calc (MaxEst x) = Max x
  {-# INLINE calc #-}



----------------------------------------------------------------

-- | Accumulator for sample mean. It uses Welford's algorithm to
--   provide numerical stability
--
--   For elements of type 'Double' it calculates mean and for elements
--   of type '(Double,Double)' it calculates weighted mean. Weight is
--   second element of the tuple.
data MeanEst = MeanEst
               {-# UNPACK #-} !Double -- Get mean estimate from accumulator.
               {-# UNPACK #-} !Double
               -- Get summary weight of elements. For unweighted mean it equal
               -- to total number of elements.
             deriving (Eq,Show,Typeable,Data)

instance FoldEstimator MeanEst Double where
  addElement (MeanEst m n) x = MeanEst m' n'
    where
      m' = m + (x - m) / n'
      n' = n + 1
  {-# INLINE addElement #-}
instance FoldEstimator MeanEst (Double,Double) where
  addElement e@(MeanEst m n) (x, w)
    | w == 0    = e
    | otherwise = MeanEst m' n'
    where
      n' = n + w
      m' = m + w * (x - m) / n'
  {-# INLINE addElement #-}

instance NullEstimator MeanEst where
  nullEstimator = MeanEst 0 0
  {-# INLINE nullEstimator #-}

instance SemigoupEst MeanEst where
   joinSample a@(MeanEst x n) b@(MeanEst y k)
     | n == 0    = a
     | k == 0    = b
     | otherwise = MeanEst ((x*n + y*k) / s) s
     where s = n + k
   {-# INLINE joinSample #-}

instance Calc MeanEst Mean where
  calc (MeanEst m _) = Mean m


----------------------------------------------------------------

-- FIXME: check estimates for weighted events. Really important!

-- | Robust estimator for variance
newtype VarianceEst = VarianceEst (Double -> MeanEst)

instance FoldEstimator VarianceEst Double where
  addElement (VarianceEst f) x = VarianceEst $ \m -> addElement (f m) (let d = x - m in d*d)
  {-# INLINE addElement #-}

instance NullEstimator VarianceEst where
  nullEstimator = VarianceEst $ \_ -> nullEstimator
  {-# INLINE nullEstimator #-}

instance SemigoupEst VarianceEst where
  joinSample (VarianceEst f) (VarianceEst g) = VarianceEst $ \m -> joinSample (f m) (g m)
  {-# INLINE joinSample #-}

instance Calc VarianceEst (Double -> Variance) where
  calc (VarianceEst est) m =
    case est m of
      MeanEst x n
        | n < 2     -> Variance   0
        | otherwise -> Variance $ x * n / (n - 1)
  {-# INLINE calc #-}

instance Calc VarianceEst (Double -> VarianceBiased) where
  calc (VarianceEst est) m =
    case est m of MeanEst x _ -> VarianceBiased x
  {-# INLINE calc #-}

instance Calc VarianceEst (Double -> StdDev) where
  calc est = StdDev . calcVariance . calc est
  {-# INLINE calc #-}

instance Calc VarianceEst (Double -> StdDevBiased) where
  calc est = StdDevBiased . calcVariance . calc est
  {-# INLINE calc #-}



----------------------------------------------------------------

data FastVar = FastVar
               {-# UNPACK #-} !Int
               {-# UNPACK #-} !Double
               {-# UNPACK #-} !Double
             deriving (Eq,Show,Typeable)


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

instance Calc FastVar Mean where
  calc (FastVar _ m _) = Mean m

instance Calc FastVar Count where
  calc (FastVar n _ _) = Count n

instance Calc FastVar Variance where
  calc (FastVar n _ s)
    | n > 1     = Variance (s / fromIntegral (n - 1))
    | otherwise = Variance  0

instance Calc FastVar VarianceBiased where
  calc (FastVar n _ s)
    | n > 1     = VarianceBiased (s / fromIntegral n)
    | otherwise = VarianceBiased  0

instance Calc FastVar StdDev where
  calc = StdDev . sqrt . calcVariance . calc

instance Calc FastVar StdDevBiased where
  calc = StdDevBiased . sqrt . calcVarianceBiased . calc



----------------------------------------------------------------

-- | Skip NaN in calculation of statistics
newtype SkipNaN m = SkipNaN { skipNaN :: m }
                    deriving (Show,Eq,Typeable)

instance (FoldEstimator m a, RealFloat a) => FoldEstimator (SkipNaN m) a where
  addElement (SkipNaN m) x
    | isNaN x   = SkipNaN   m
    | otherwise = SkipNaN $ addElement m x
  {-# INLINE addElement #-}

instance NullEstimator m => NullEstimator (SkipNaN m) where
  nullEstimator = SkipNaN nullEstimator
  {-# INLINE nullEstimator #-}

instance SemigoupEst m => SemigoupEst (SkipNaN m) where
  joinSample (SkipNaN m) (SkipNaN n) = SkipNaN (joinSample m n)
  {-# INLINE joinSample #-}

instance Calc m r => Calc (SkipNaN m) r where
  calc = calc . skipNaN
