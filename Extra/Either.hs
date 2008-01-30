module Extra.Either where

import Data.Maybe

lefts :: [Either a b] -> [a]
lefts xs = fst (partitionEithers xs)

rights :: [Either a b] -> [b]
rights xs = snd (partitionEithers xs)

isRight (Right _) = True
isRight (Left _) = False

isLeft = not . isRight

-- |Turn a list of eithers into an either of lists
concatEithers :: [Either a b] -> Either [a] [b]
concatEithers xs =
    case partitionEithers xs of 
      ([], rs) -> Right rs
      (ls, _) -> Left ls

-- |Return a pair of the lefts and the rights
partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers xs =
    part ([], []) (reverse xs)
    where
      part (ls, rs) [] = (ls, rs)
      part (ls, rs) (Left l : more) = part (l : ls, rs) more
      part (ls, rs) (Right r : more) = part (ls, r : rs) more

-- Deprecated
rightOnly = rights
eitherFromList = concatEithers
