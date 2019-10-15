-- | Pretty print Loc info for stack traces

{-# LANGUAGE CPP, FlexibleInstances, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS -Wall -Wno-orphans #-}

module Extra.TH
    ( here
    , Loc
    , noLoc
    , prettyLocs
    , addConstraints
    , addConstraint
    , removeConstraints
    ) where

import Control.Lens (_1, over)
import Data.Generics (everywhere, mkT)
import Data.List (intersperse, nub)
import Data.Map as Map (findWithDefault, fromList, fromListWith, lookup, Map, toList)
import Data.Maybe (mapMaybe)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import Data.Tuple (swap)
import Extra.Generics (gFind)
import Language.Haskell.TH (appT, conT, Dec(InstanceD), ExpQ, Loc(..), location, Name, nameBase, newName, Q, Type(..), TypeQ, varT)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift (lift)
import Text.PrettyPrint.HughesPJClass (Doc, hcat, Pretty(..), text)

here :: ExpQ
here = lift =<< location

instance Pretty Loc where
    pPrint = prettyLoc

noLoc :: Loc
noLoc = Loc "" "" "" (0, 0) (0, 0)

prettyLoc :: Loc -> Doc
prettyLoc (Loc _filename _package modul (line, col) _) = text (modul <> ":" ++ show line ++ ":" ++ show col)

prettyLocs :: [Loc] -> Doc
prettyLocs locs = text "[" <> hcat (intersperse (text " ← ") (fmap prettyLoc locs)) <> text "]"

-- | Add some simple constraints to a 'Dec'. The Dec is typically one
-- emitted by a function such as makeAcidic or deriveSafeCopy.
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

-- | Unify the type variables in the existing constraints with those
-- in the new constraint based on nameBase.  If the type variable does
-- not appear in the declaration it is an error.
addConstraint :: Name -> TypeQ -> Dec -> Q Dec
addConstraint tname typeq (InstanceD mo cxt inst decs) = do
  let vnames = nub $ mapMaybe isVar (gFind (inst : cxt) :: [Type])
      mp :: Map String Name
      mp = Map.fromList (fmap (\n -> (nameBase n, n)) vnames)
  (extra :: Type) <- everywhere (mkT (fixTypeVar mp)) <$> appT (conT tname) typeq
  return $ InstanceD mo (cxt ++ [extra]) inst decs
    where
      isVar :: Type -> Maybe Name
      isVar (VarT name) = Just name
      isVar _ = Nothing
      fixTypeVar :: Map String Name -> Type -> Type
      fixTypeVar mp (VarT n) = VarT (maybe n id (Map.lookup (nameBase n) mp))
      fixTypeVar _mp t = t
addConstraint _ _ d = return d

-- | Remove simple constraints from a 'Dec' based on type class name
-- and type variable name.
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
