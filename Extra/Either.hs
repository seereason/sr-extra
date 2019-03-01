module Extra.Either where

import Data.Either

-- |Turn a list of eithers into an either of lists
concatEithers :: [Either a b] -> Either [a] [b]
concatEithers xs =
    case partitionEithers xs of
      ([], rs) -> Right rs
      (ls, _) -> Left ls
{-# DEPRECATED concatEithers "This is terrible.  Delete your account." #-}
