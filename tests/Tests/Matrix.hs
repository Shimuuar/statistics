module Tests.Matrix (tests) where

import Statistics.Matrix hiding (map)
import Statistics.Matrix.Algorithms
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Tests.ApproxEq (ApproxEq(..))
import Tests.Matrix.Types
import qualified Data.Vector.Unboxed as U

t_row :: Mat Double -> Gen Property
t_row ms@(Mat r _ xs) = do
  i <- choose (0,r-1)
  return $ row (fromMat ms) i === U.fromList (xs !! i)

t_column :: Mat Double -> Gen Property
t_column ms@(Mat _ c xs) = do
  i <- choose (0,c-1)
  return $ column (fromMat ms) i === U.fromList (map (!! i) xs)

t_center :: Mat Double -> Property
t_center ms@(Mat r c xs) =
  (xs !! (r `quot` 2)) !! (c `quot` 2) === center (fromMat ms)

t_transpose :: Matrix -> Property
t_transpose m = U.concat (map (column n) [0..rows m-1]) === toVector m
  where n = transpose m

-- | Test that A = QR
t_qr :: Matrix -> Property
t_qr a
  = hasNaN p .||.
    ( counterexample ("Q=\n"++show q)
    $ counterexample ("R=\n"++show r)
    $ conjoin [ eql 1e-10 a p
              , property $ isUpperTri r
              ]
    )
  where
    (q,r) = qr a
    p     = q `multiply` r


-- | Test that Q is indeed orthogonal
t_qr_ortho :: Square Matrix -> Property
t_qr_ortho (Square a)
  = hasNaN p .||.
    ( counterexample ("Q=\n"++show q)
    $ counterexample ("R=\n"++show r)
    $ conjoin
      [ counterexample ("I=\n"++show id1) $ isIdentity id1
      , counterexample ("I=\n"++show id2) $ isIdentity id2
      ])
  where
    (q,r) = qr a
    p     = q `multiply` r
    -- Approximately identity matrices
    id1   = q `multiply` transpose q
    id2   = transpose q `multiply` q


isIdentity :: Matrix -> Bool
isIdentity m
  | r /= c    = error "Matrix is mot square!"
  | otherwise = and [ if i == j then abs (x - 1) < 1e-12 else abs x < 1e-12
                    | i <- [0 .. r-1]
                    , j <- [0 .. r-1]
                    , let x = unsafeIndex m i j
                    ]
  where
    (r,c) = dimension m

isUpperTri :: Matrix -> Bool
isUpperTri m
  | r /= c    = error "Matrix is mot square!"
  | otherwise = and [ x == 0
                    | i <- [0 .. r-1]
                    , j <- [0 .. i-1]
                    , let x = unsafeIndex m i j
                    ]
  where
    (r,c) = dimension m

tests :: Test
tests = testGroup "Matrix" [
    testProperty "t_row" t_row
  , testProperty "t_column" t_column
  , testProperty "t_center" t_center
  , testProperty "t_transpose" t_transpose
  , testProperty "t_qr" t_qr
  , testProperty "t_qr_ortho" t_qr_ortho
  ]
