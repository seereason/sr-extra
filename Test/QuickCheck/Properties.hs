module Test.QuickCheck.Properties where

import Test.QuickCheck

isIdempotentBy :: ({-Arbitrary a,-} Eq a, Show a) => (a -> a) -> Gen a -> Property
isIdempotentBy f src = forAll src $ \a -> f a == f (f a)

isIdempotent :: (Arbitrary a, Eq a, Show a) => (a -> a) -> Property
isIdempotent f = isIdempotentBy f arbitrary
