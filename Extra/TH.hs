-- | Pretty print Loc info for stack traces

{-# LANGUAGE CPP, FlexibleInstances, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS -Wall -Wno-orphans #-}

module Extra.TH
    ( here
    , Loc
    , prettyLocs
    , addConstraints
    , removeConstraints
    ) where

import Control.Lens (_1, over)
import Data.List (intersperse)
import Data.Map as Map (findWithDefault, fromListWith, Map, toList)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Tuple (swap)
import Language.Haskell.TH (appT, conT, Dec(InstanceD), ExpQ, Loc(..), location, Name, nameBase, newName, Q, Type(..), varT)
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

-- | Add some constraints to a 'Dec', typically one emitted by a
-- function such as makeAcidic or deriveSafeCopy:
addConstraints :: [(Name, String)] -> Dec -> Q Dec
addConstraints cs (InstanceD mo cxt typ decs) = do
  let mp :: Map String [Name]
      mp = Map.fromListWith (<>) (fmap (swap . over _1 (: [])) cs)
  (extra :: [Type]) <- concat <$> mapM (uncurry constrain) (Map.toList mp)
  return $ InstanceD mo (cxt ++ extra) typ decs
    where
      -- Apply some type constraints to a type variable
      constrain vstring tnames = do
        vname <- newName vstring
        mapM (\tname -> appT (conT tname) (varT vname)) tnames
addConstraints _ d = return d

removeConstraints :: [(Name, String)] -> Dec -> Q Dec
removeConstraints cs (InstanceD mo cxt typ decs) = do
  let mp :: Map String [Name]
      mp = Map.fromListWith (<>) (fmap (swap . over _1 (: [])) cs)
  return $ InstanceD mo (filter (testPred mp) cxt) typ decs
    where
      testPred :: Map String [Name] -> Type -> Bool
      -- Discard any predicate that matches a map element
      testPred mp (AppT (ConT tname) (VarT vname)) =
          not (elem tname (Map.findWithDefault [] (nameBase vname) mp))
      testPred _ _ = True -- keep anything else
removeConstraints _ d = return d
