module Extra.Either
    ( rightOnly
    , eitherFromList
    ) where

import Data.Maybe

rightOnly :: [Either a b] -> [b]
rightOnly = catMaybes . map (either (const Nothing) Just)

-- |Turn a list of eithers into an either of lists
eitherFromList :: [Either a b] -> Either [a] [b]
eitherFromList l =
    merge [] [] l
    where
      merge [] r [] = Right r
      merge l _ [] = Left l
      merge l r (Left x : etc) = merge (x : l) r etc
      merge l r (Right x : etc) = merge l (x : r) etc
