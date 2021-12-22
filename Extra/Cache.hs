module Extra.Cache (HasLens(hasLens)) where

import Control.Lens (Lens')

-- This class can unify many Has* classes, but separate classes have
-- the advantage that you could use different class names to target
-- different fields in s of the same type.  Not a very compelling advantage.
class HasLens v s where
  hasLens :: Lens' s v
