{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module    : Statistics.Regression
-- Copyright : 2014 Bryan O'Sullivan
-- License   : BSD3
--
-- Functions for regression analysis.

module Statistics.Regression
    ( LinearModel(..)
    , ols
    , olsModel
    , olsRegress
    , olsMatrix
    , calculateRSquare
    -- , bootstrapRegress
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.DeepSeq (rnf)
import Control.Monad (forM_, replicateM)
import GHC.Conc (getNumCapabilities)
import Prelude hiding (pred, sum)
import Statistics.Function as F
import Statistics.Matrix hiding (map)
import Statistics.Matrix.Algorithms (qr,solve)
import Statistics.Resampling (splitGen)
import Statistics.Resampling.Bootstrap (Estimate(..))
import Statistics.Sample (mean)
import Statistics.Sample.Internal (sum)
import System.Random.MWC (GenIO, uniformR)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import           Data.Vector.Generic   ((!))
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M


----------------------------------------------------------------
-- Ordinary least squares without errors
----------------------------------------------------------------

-- | Linear model for ordinary least squares fit
data LinearModel a = LinearModel
  { linearModel  :: a -> Double
    -- ^ Evaluate model prediction at the point
  , linearCoeffs :: Vector
    -- ^ Coefficients of model
  , rSquare      :: !Double
    -- ^ R-square goodness of fit criterion.
  }

-- | Perform ordinary least-squares linear regression on set of (x,y)
--   points.
ols :: G.Vector v (Double,Double) => v (Double,Double) -> LinearModel Double
ols points
  | n < 2     = error "Too few points to perform linear regression"
  | otherwise = LinearModel
              { linearModel  = \x -> (coeffs ! 0) * x + (coeffs ! 1)
              , linearCoeffs = coeffs
              , rSquare      = calculateRSquare mat ys coeffs
              }
  where
    n       = G.length points
    (xs,ys) = U.unzip $ G.convert points
    mat     = fromColumns [ xs
                          , U.replicate n 1
                          ]
    coeffs  = olsMatrix mat ys

-- | General linear model for data.
olsModel :: (G.Vector v a, G.Vector v Double)
         => [a -> Double]       -- ^ Functions
         -> v a                 -- ^ Predictor vector
         -> v Double            -- ^ Responder vector
         -> LinearModel a
olsModel [] _ _ = error "Not enough functions"
olsModel funs points ys
  | n < nCol  = error "Not enough data to evaluate model"
  | otherwise = LinearModel
              { linearModel = \a -> G.sum $ G.zipWith (*) coeffs (G.fromList [f a | f <- funs])
              , linearCoeffs = coeffs
              , rSquare      = calculateRSquare mat (G.convert ys) coeffs
              }
  where
    nCol   = length funs
    n      = G.length points
    mat    = fromColumns [ G.convert (G.map f points) | f <- funs ]
    coeffs = olsMatrix mat (G.convert ys)

-- | Perform an ordinary least-squares regression on a set of
-- predictors, and calculate the goodness-of-fit of the regression.
--
-- The returned pair consists of:
--
-- * A vector of regression coefficients.  This vector has /one more/
--   element than the list of predictors; the last element is the
--   /y/-intercept value.
--
-- * /R&#0178;/, the coefficient of determination (see 'calculateRSquare' for
--   details).
olsRegress :: [Vector]
              -- ^ Non-empty list of predictor vectors.  Must all have
              -- the same length.  These will become the columns of
              -- the matrix /A/ solved by 'ols'.
           -> Vector
              -- ^ Responder vector.  Must have the same length as the
              -- predictor vectors.
           -> LinearModel Vector
olsRegress preds@(_:_) resps
  | any (/=n) ls        = error $ "predictor vector length mismatch " ++
                                  show lss
  | G.length resps /= n = error $ "responder/predictor length mismatch " ++
                                  show (G.length resps, n)
  | otherwise           = LinearModel
                        { linearModel   = \v -> case () of
                             _| G.length v /= nCol -> error "Invalid number of columns"
                              | otherwise          -> sum $ G.zipWith (*) (G.snoc v 1) (coeffs)
                        , linearCoeffs  = coeffs
                        , rSquare       = calculateRSquare mxpreds resps coeffs
                        }
  where
    nCol      = length preds
    coeffs    = olsMatrix mxpreds resps
    mxpreds   = transpose .
                fromVector (length lss + 1) n .
                G.concat $ preds ++ [G.replicate n 1]
    lss@(n:ls) = map G.length preds
olsRegress _ _ = error "no predictors given"

-- | Compute the ordinary least-squares solution to /A x = b/.
olsMatrix
    :: Matrix     -- ^ /A/ has at least as many rows as columns.
    -> Vector     -- ^ /b/ has the same length as columns in /A/.
    -> Vector
olsMatrix a b
  | rs < cs   = error $ "fewer rows than columns " ++ show d
  | otherwise = solve r (transpose q `multiplyV` b)
  where
    d@(rs,cs) = dimension a
    (q,r)     = qr a


-- | Compute /R&#0178;/, the coefficient of determination that
-- indicates goodness-of-fit of a regression.
--
-- This value will be 1 if the predictors fit perfectly, dropping to 0
-- if they have no explanatory power.
calculateRSquare :: Matrix               -- ^ Predictors (regressors).
        -> Vector               -- ^ Responders.
        -> Vector               -- ^ Regression coefficients.
        -> Double
calculateRSquare pred resp coeff = 1 - r / t
  where
    r   = sum $ flip U.imap resp $ \i x -> square (x - p i)
    t   = sum $ flip U.map resp $ \x -> square (x - mean resp)
    p i = sum . flip U.imap coeff $ \j -> (* unsafeIndex pred i j)

{-
-- | Bootstrap a regression function.  Returns both the results of the
-- regression and the requested confidence interval values.
bootstrapRegress :: GenIO
                 -> Int         -- ^ Number of resamples to compute.
                 -> Double      -- ^ Confidence interval.
                 -> ([Vector] -> Vector -> (Vector, Double))
                 -- ^ Regression function.
                 -> [Vector]    -- ^ Predictor vectors.
                 -> Vector      -- ^ Responder vector.
                 -> IO (V.Vector Estimate, Estimate)
bootstrapRegress gen0 numResamples ci rgrss preds0 resp0
  | numResamples < 1   = error $ "bootstrapRegress: number of resamples " ++
                                 "must be positive"
  | ci <= 0 || ci >= 1 = error $ "bootstrapRegress: confidence interval " ++
                                 "must lie between 0 and 1"
  | otherwise = do
  caps <- getNumCapabilities
  gens <- splitGen caps gen0
  done <- newChan
  forM_ (zip gens (balance caps numResamples)) $ \(gen,count) -> do
    forkIO $ do
      v <- V.replicateM count $ do
           let n = U.length resp0
           ixs <- U.replicateM n $ uniformR (0,n-1) gen
           let resp  = U.backpermute resp0 ixs
               preds = map (flip U.backpermute ixs) preds0
           return $ rgrss preds resp
      rnf v `seq` writeChan done v
  (coeffsv, r2v) <- (G.unzip . V.concat) <$> replicateM caps (readChan done)
  let coeffs  = flip G.imap (G.convert coeffss) $ \i x ->
                est x . U.generate numResamples $ \k -> ((coeffsv G.! k) G.! i)
      r2      = est r2s (G.convert r2v)
      (coeffss, r2s) = rgrss preds0 resp0
      est s v = Estimate s (w G.! lo) (w G.! hi) ci
        where w  = F.sort v
              lo = round c
              hi = truncate (n - c)
              n  = fromIntegral numResamples
              c  = n * ((1 - ci) / 2)
  return (coeffs, r2)

-- | Balance units of work across workers.
balance :: Int -> Int -> [Int]
balance numSlices numItems = zipWith (+) (replicate numSlices q)
                                         (replicate r 1 ++ repeat 0)
 where (q,r) = numItems `quotRem` numSlices
-}
