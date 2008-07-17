-----------------------------------------------------------------------------
-- |
-- Module      :  Test.QUnit
-- Copyright   :  (c) Koen Claessen, John Hughes 2001, Jeremy Shaw 2008
-- License     :  BSD-style (see the file libraries\/base\/LICENSE)
-- 
-- Maintainer  :  jeremy\@n-heptane.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Some glue code for running QuickCheck tests using the HUnit framework.
--
-- This module provides an instance of Test.HUnit.Testable for
-- Test.QuickCheck.Property, which makes it trivial to use QuickCheck
-- properties in the HUnit framework:
--
-- @
--   runTestTT $ (\"x \/= x\" ~: property (\x -> x /= x))
-- @
--
-- The QuickCheck Property will be run using
-- Test.QuickCheck.defaultConfig.  If you need to specific an
-- alternate config, then use 'testQuickCheck' like this:
--
-- @
--   runTestTT $ (\"x \/= x\" ~: testQuickCheck myConfig (\x -> x /= x))
-- @
-----------------------------------------------------------------------------
module Test.QUnit (testQuickCheck) where

import System.Random
import Test.HUnit as HU
import Test.QuickCheck as QC

-- |an instance of Test.HUnit.Testable for Test.QuickCheck.Property
--
-- Note: I did not add an instance:
--
-- instance (QC.Testable a) => (HU.Testable a)
--
-- Because it results in undeciable instances. For example, there is
-- an instance of 'Bool' for QC.Testable and HU.Testable already.
instance HU.Testable Property where
    test qc = testQuickCheck defaultConfig qc

-- |turns the quickcheck test into an hunit test
--
-- Use this if you want to provide a custom 'Config' instead of
-- 'defaultConfig'.
testQuickCheck :: (QC.Testable a) => 
           Config -- ^ quickcheck config
        -> a      -- ^ quickcheck property
        -> Test
testQuickCheck config property =
    TestCase $ do rnd <- newStdGen
                  tests config (evaluate property) rnd 0 0 []

-- |modified version of the tests function from Test.QuickCheck
tests :: Config -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO () 
tests config gen rnd0 ntest nfail stamps
  | ntest == configMaxTest config = return () 
  | nfail == configMaxFail config = assertFailure $ "Arguments exhausted after " ++ show ntest ++ " tests."
  | otherwise               =
      do putStr (configEvery config ntest (arguments result))
         case ok result of
           Nothing    ->
             tests config gen rnd1 ntest (nfail+1) stamps
           Just True  ->
             tests config gen rnd1 (ntest+1) nfail (stamp result:stamps)
           Just False ->
             assertFailure $  ( "Falsifiable, after "
                   ++ show ntest
                   ++ " tests:\n"
                   ++ unlines (arguments result)
                    )
     where
      result      = generate (configSize config ntest) rnd2 gen
      (rnd1,rnd2) = split rnd0
