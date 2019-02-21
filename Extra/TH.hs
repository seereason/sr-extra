-- | Pretty print Loc info for stack traces

{-# LANGUAGE CPP, FlexibleInstances, TemplateHaskell #-}
{-# OPTIONS -Wall -Wno-orphans #-}

module Extra.TH
    ( here
    , Loc
    ) where

import Data.List (intersperse)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Language.Haskell.TH (ExpQ, Loc(..), location)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift (lift)
import Text.PrettyPrint.HughesPJClass (Doc, hcat, Pretty(..), text)

here :: ExpQ
here = lift =<< location

instance Pretty Loc where
    pPrint = prettyLoc

prettyLoc :: Loc -> Doc
prettyLoc (Loc _filename _package modul (line, col) _) = text (modul <> ":" ++ show line ++ ":" ++ show col)

prettyLocs :: [Loc] -> Doc
prettyLocs locs = text "[" <> hcat (intersperse (text " ← ") (fmap prettyLoc locs)) <> text "]"
