module Extra.URIQuery
    ( modify
    , del
    , put
    , copy
    ) where

import Data.List (partition)
import Data.Maybe (listToMaybe)
import Extra.URI

-- |Modify an individual URI query attributes.
modify :: String -> (Maybe String -> Maybe String) -> URI -> URI
modify a vf uri =
    let (vs, other) = partition ((== a) . fst) (parseURIQuery uri) in
    setURIQuery (case vf (listToMaybe (map snd vs)) of
                   Just v' -> (a, v') : other
                   Nothing -> other) uri

-- |Replace a query attribute with Nothing.
del :: String -> URI -> URI
del a uri = modify a (const Nothing) uri

-- |Replace a query attribute with something.
put :: String -> String -> URI -> URI 
put a v uri = modify a (const (Just v)) uri

-- |Copy an attribute from one query to another
copy :: String -> URI -> URI -> URI
copy a src dst = modify a (const (lookup a (parseURIQuery src))) dst

-- Apply f to all of the URI's pairs
{-
modifyAll :: (String -> Maybe String -> Maybe String) -> URI -> URI
modifyAll f uri =
    foldr (\ (a, _) uri -> modify a (f a) uri) uri pairs
    where
      pairs = parseURIQuery uri
-}
