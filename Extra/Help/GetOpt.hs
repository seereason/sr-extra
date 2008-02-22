module Extra.Help.GetOpt 
    ( OptDescr(..)
    , ArgDescr(..)
    , ArgOrder(..)
    , getOpt
    ) where

import Extra.Help.Markup
import System.Console.GetOpt (ArgOrder(..), ArgDescr(..))
import qualified System.Console.GetOpt as G

data OptDescr a = Option [Char] [String] (ArgDescr a) Text

getOpt :: ArgOrder a -> [OptDescr a] -> [String] -> ([a], [String], [String])
getOpt argOrder optDescr args = G.getOpt argOrder (map removeMarkup optDescr) args
    where
      removeMarkup :: OptDescr a -> G.OptDescr a
      removeMarkup (Option shortOpts longOpts argDescr descr) =
          G.Option shortOpts longOpts argDescr (textToString descr)
