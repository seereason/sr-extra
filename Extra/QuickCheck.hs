module Extra.QuickCheck
  ( throwResult
  ) where

import Control.Exception
import Test.QuickCheck
import Data.Semigroup

instance Exception Result

instance Semigroup Result where
  (Success numTests1 numDiscarded1 labels1 classes1 tables1 output1) <>
      (Success numTests2 numDiscarded2 labels2 classes2 tables2 output2) =
    Success (numTests1 + numTests2) (numDiscarded1 + numDiscarded2) (labels1 <> labels2)
            (classes1 <> classes2) (tables1 <> tables2) (output1 <> "\n" <> output2)
  (Success _ _ _ _ _ _) <> failure = failure
  failure <> _ = failure

instance Monoid Result where
  mempty = Success 0 0 mempty mempty mempty mempty
  mappend = (<>)

throwResult :: Result -> IO Result
throwResult result@(Success {}) = return result
throwResult result = throw result

{-
Example use:

tests :: IO Result
tests = do
  mconcat <$> sequence
       [quickCheckResult prop_next,
        quickCheckResult prop_keys,
        quickCheckResult prop_next,
        quickCheckResult prop_lookup,
        quickCheckResult prop_lookupKey,
        quickCheckResult prop_lookupPair,
        quickCheckResult prop_splitAt,
        quickCheckResult prop_uncons,
        quickCheckResult prop_null,
        quickCheckResult prop_singleton] >>= throwResult
-}
