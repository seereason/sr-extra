{-# LANGUAGE GADTs, RankNTypes, ScopedTypeVariables #-}

import Data.Foldable
import Data.Map as Map (insert, Map)

-- | The 'nubBy' function behaves just like 'nub', except it uses a
-- user-supplied equality predicate instead of the overloaded '=='
-- function.
--
-- >>> nubBy (\x y -> mod x 3 == mod y 3) [1,2,4,5,6]
-- [1,2,6]
nubBy ::
    forall t a. (Foldable t, Functor t, Monoid (t a))
    => (a -> t a -> t a) -> (a -> a -> Bool) -> t a -> t a
nubBy cons eq l =
  -- There must be some way to avoid having to reverse this.
  rev $ foldl' go mempty l
  where
    go :: t a -> a -> t a
    -- Note that we keep the call to `eq` with arguments in the
    -- same order as in the reference (prelude) implementation,
    -- and that this order is different from how `elem` calls (==).
    -- See #2528, #3280 and #7913.
    -- 'xs' is the list of things we've seen so far,
    -- 'y' is the potential new element
    go xs y | any (eq y) xs = xs
    go xs y = cons y xs
    rev :: t a -> t a
    rev xs = foldl' (flip cons) mempty xs

mapFromFoldable :: (Foldable t, Ord k) => t (k, a) -> Map k a
mapFromFoldable t = foldl' f mempty t
    where f m (k, a) = Map.insert k a m

-- | Convert any Foldable to some other Foldable, given an insert
-- function and provided the destination type is a monoid.
--
-- > fromFoldable (<|) [1,2,3] :: Seq Int
-- fromList [1,2,3]
-- > fromFoldable (flip (|>)) [1,2,3] :: Seq Int
-- fromList [3,2,1]
fromFoldable ::
    (Foldable t, Foldable t', Monoid (t' a))
    => (a -> t' a -> t' a) -> t a -> t' a
fromFoldable ins t = foldr ins mempty t
