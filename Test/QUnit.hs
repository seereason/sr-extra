{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Test.QUnit
-- Copyright   :  (c) Koen Claessen, John Hughes 2001, Jeremy Shaw 2008
-- License     :  BSD-style (see the file libraries\/base\/LICENSE)
-- 
-- Maintainer  :  jeremy@n-heptane.com
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
--   import Test.HUnit
--   import Test.HUnit.Text
--   import Test.QuickCheck
--   import Test.QUnit
--
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

import qualified Test.HUnit as HU
import qualified Test.QuickCheck as QC

-- |an instance of Test.HUnit.Testable for Test.QuickCheck.Property
--
-- Note: I did not add an instance:
--
-- instance (QC.Testable a) => (HU.Testable a)
--
-- Because it results in undeciable instances. For example, there is
-- an instance of 'Bool' for QC.Testable and HU.Testable already.
instance HU.Testable QC.Property where
    test qc = testQuickCheck QC.stdArgs qc

-- |turns the quickcheck test into an hunit test
--
-- Use this if you want to provide a custom 'Config' instead of
-- 'defaultConfig'.
testQuickCheck :: (QC.Testable a) => 
           QC.Args  -- ^ quickcheck config
        -> a        -- ^ quickcheck property
        -> HU.Test
testQuickCheck args prop =
    HU.TestCase $
    do result <- QC.quickCheckWithResult args prop
       case result of
         QC.Success{} -> return ()
         QC.GaveUp{} -> let ntest = QC.numTests result in HU.assertFailure $ "Arguments exhausted after" ++ show ntest ++ (if ntest == 1 then " test." else " tests.")
         QC.Failure{} -> let reason = QC.reason result in HU.assertFailure reason
         QC.NoExpectedFailure{} -> HU.assertFailure $ "No Expected Failure"
