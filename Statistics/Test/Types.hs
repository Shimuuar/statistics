{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module    : Statistics.Distribution
-- Copyright : (c) 2012-2013 Alekey Khudyakov
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Data types and functions common for the statistical tests.
module Statistics.Test.Types (
    -- * Test results
    TestResult(..)
  , testIsSignificant
  , testSignificance
  , TestSignificance(..)
  , significant
    -- * Common data types
  , TestType(..)
  ) where

import Data.Data (Typeable,Data)


-- | Result of the statistical test
data TestResult = TestResult
  { observedSignificance  :: {-# UNPACK #-} !Double
    -- ^ P-value obtained from test.
  , requestedSignificance :: {-# UNPACK #-} !Double
    -- ^ P-value which was requested during test.
  }
  deriving (Show,Eq,Typeable,Data)


-- | Is test result significant or not.
testIsSignificant :: TestResult -> Bool
testIsSignificant (TestResult pObs pReq)
  = pObs < pReq

-- | Is test result significant or not.
testSignificance :: TestResult -> TestSignificance
testSignificance t
  = if testIsSignificant t then Significant else NotSignificant


-- | Result of hypothesis testing
data TestSignificance
  = Significant    -- ^ Null hypothesis should be rejected
  | NotSignificant -- ^ Data is compatible with hypothesis
  deriving (Eq,Ord,Show,Typeable,Data)

-- | Convert between booleans and significance. 'True' means 'Significant'.
significant :: Bool -> TestSignificance
significant True  = Significant
significant False = NotSignificant


-- | Test type. Exact meaning depends on a specific test. But
--   generally it's tested whether some statistics is too big (small)
--   for 'OneTailed' or whether it too big or too small for 'TwoTailed'
data TestType = OneTailed
              | TwoTailed
              deriving (Eq,Ord,Show,Typeable)
