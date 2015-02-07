-- |
-- Module    : Statistics.Matrix.Algorithms
-- Copyright : 2014 Bryan O'Sullivan
-- License   : BSD3
--
-- Useful matrix functions.

module Statistics.Matrix.Algorithms
    (
      qr
    , solve  
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad.ST (ST, runST)
import Prelude hiding (sum, replicate)
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as M

import           Statistics.Matrix
import qualified Statistics.Matrix.Mutable as MM
import Statistics.Sample.Internal (sum)
import Statistics.Function as F


-- | /O(r*c)/ Compute the QR decomposition of a matrix.
-- The result returned is the matrices (/q/,/r/).
qr :: Matrix -> (Matrix, Matrix)
qr mat = runST $ do
  let (m,n) = dimension mat
  r <- MM.replicate n n 0
  a <- MM.thaw mat
  for 0 n $ \j -> do
    cn <- MM.immutably a $ \aa -> norm (column aa j)
    MM.unsafeWrite r j j cn
    for 0 m $ \i -> MM.unsafeModify a i j (/ cn)
    for (j+1) n $ \jj -> do
      p <- innerProduct a j jj
      MM.unsafeWrite r j jj p
      for 0 m $ \i -> do
        aij <- MM.unsafeRead a i j
        MM.unsafeModify a i jj $ subtract (p * aij)
  (,) <$> MM.unsafeFreeze a <*> MM.unsafeFreeze r

innerProduct :: MM.MMatrix s -> Int -> Int -> ST s Double
innerProduct mmat j k = MM.immutably mmat $ \mat ->
  sum $ U.zipWith (*) (column mat j) (column mat k)


-- | Solve the equation /R x = b/.
solve :: Matrix     -- ^ /R/ is an upper-triangular square matrix.
      -> Vector     -- ^ /b/ is of the same length as rows\/columns in /R/.
      -> Vector
solve r b
  | n /= l    = error $ "row/vector mismatch " ++ show (n,l)
  | otherwise = U.create $ do
  s <- U.thaw b
  rfor n 0 $ \i -> do
    si <- (/ unsafeIndex r i i) <$> M.unsafeRead s i
    M.unsafeWrite s i si
    for 0 i $ \j -> F.unsafeModify s j $ subtract ((unsafeIndex r j i) * si)
    return ()
  return s
  where n = rows r
        l = U.length b
