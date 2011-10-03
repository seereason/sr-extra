module Extra.Either where

isRight (Right _) = True
isRight (Left _) = False

isLeft = not . isRight

-- |Turn a list of eithers into an either of lists
concatEithers :: [Either a b] -> Either [a] [b]
concatEithers xs =
    case partitionEithers xs of 
      ([], rs) -> Right rs
      (ls, _) -> Left ls

{-# DEPRECATED rightOnly "Use rights" #-}
rightOnly = rights
{-# DEPRECATED eitherFromList "Use concatEithers" #-}
eitherFromList = concatEithers
