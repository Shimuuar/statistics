{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tests.Matrix.Types
    (
      Mat(..)
    , fromMat
    -- , toMat
    , Square(..)
    ) where

import Control.Monad (join)
import Control.Applicative ((<$>), (<*>))
import Statistics.Matrix (Matrix(..), fromList)
import Test.QuickCheck
import Tests.Helpers (shrinkFixedList, small)
import qualified Data.Vector.Unboxed as U
import Debug.Trace

----------------------------------------------------------------
-- Matrix instances
----------------------------------------------------------------

-- | Matrix represented as rows
data Mat a = Mat { mrows :: Int , mcols :: Int
                 , asList :: [[a]] }
              deriving (Eq, Ord, Show, Functor)

fromMat :: Mat Double -> Matrix
fromMat (Mat r c xs) = fromList r c (concat xs)

-- toMat :: Matrix -> Mat Double
-- toMat m = Mat (rows m) (cols m) (toRowLists m)


instance (Arbitrary a) => Arbitrary (Mat a) where
    arbitrary = small $ join (arbMat <$> arbitrary <*> arbitrary)
    shrink (Mat r c xs) = Mat r c <$> shrinkFixedList (shrinkFixedList shrink) xs

arbMat :: (Arbitrary a) => Positive (Small Int) -> Positive (Small Int)
       -> Gen (Mat a)
arbMat (Positive (Small r)) (Positive (Small c)) =
    Mat r c <$> vectorOf r (vector c)

instance Arbitrary Matrix where
    arbitrary = fromMat <$> arbitrary
    -- shrink    = map fromMat . shrink . toMat

----------------------------------------------------------------
-- Square matrices
----------------------------------------------------------------

-- | Square matrix
newtype Square m = Square { asSquare :: m }
                   deriving (Show,Functor)

instance Arbitrary a => Arbitrary (Square (Mat a)) where
  arbitrary = do
    Positive (Small n) <- arbitrary
    traceShow n $ return ()
    xs      <- vectorOf n (vector n)
    return $ Square $ Mat n n xs

instance Arbitrary (Square Matrix) where
  arbitrary = do
    m <- arbitrary
    traceShow m $ return ()
    return $ fmap fromMat m
