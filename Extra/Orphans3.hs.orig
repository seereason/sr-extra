-- | Serialize and SafeCopy instances for the template haskell Exp type.
-- Some Arbitrary, Lift, and Ppr instances.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall -Wredundant-constraints -Wno-orphans #-}

module Extra.Orphans3 where

import Data.Data (Data, dataTypeOf, gunfold, mkNoRepType, toConstr, TypeRep)
import Data.Generics.Instances ()
import Data.ListLike as LL hiding (sequence, toList)
import Data.Proxy (Proxy(Proxy))
import Data.SafeCopy (SafeCopy(..))
import Extra.Serialize (Serialize)
import Extra.Orphans ()
#if MIN_VERSION_template_haskell(2,16,0)
import GHC.Word
#endif
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift
import Language.Haskell.TH.PprLib (Doc, hcat, ptext)
import Language.Haskell.TH.Syntax
import Prelude hiding (foldl1)
#if !__GHCJS__
import Network.URI
import Test.QuickCheck (Arbitrary(arbitrary), elements, Gen, oneof)
#endif

pprPair :: (Ppr a, Ppr b) => (a, b) -> Doc
pprPair (a, b) = hcat [ptext "(", ppr a, ptext ",", ppr b, ptext ")"]

pprList :: [Doc] -> Doc
pprList xs = hcat [ptext "[", hcat (intersperse (ptext ",") xs), ptext "]"]

#if 0
instance (Ppr k, Ord k, Enum k, Show k, Ppr v) => Ppr (Order k v) where
  ppr = pprList . toList . fmap pprPair . toPairs
#endif

#if __GLASGOW_HASKELL__ >= 802
instance Data TypeRep where
  toConstr _   = error "toConstr"
  gunfold _ _  = error "gunfold"
  dataTypeOf _ = mkNoRepType "Data.Typeable.TypeRep"
#endif

deriving instance Serialize (Proxy a)

#if !__GHCJS__
$(deriveLift ''Proxy)

instance Arbitrary NameSpace where
    arbitrary = elements [VarName, DataName, TcClsName]

instance Arbitrary Type where
    arbitrary = oneof [ ForallT <$> arbitrary <*> arbitrary <*> arbitrary
                      , AppT <$> arbitrary <*> arbitrary
                      , SigT <$> arbitrary <*> arbitrary
                      , VarT <$> arbitraryTypeVariableName
                      , ConT <$> arbitrary
                      , PromotedT <$> arbitrary
                      , TupleT <$> arbitrary
                      , UnboxedTupleT <$> arbitrary
                      , pure ArrowT
                      , pure EqualityT
                      , pure ListT
                      , PromotedTupleT <$> arbitrary
                      , pure PromotedNilT
                      , pure PromotedConsT
                      , pure StarT
                      , pure ConstraintT
                      , LitT <$> arbitrary ]

instance Arbitrary (Proxy a) where arbitrary = elements [Proxy]
instance Arbitrary Name where arbitrary = pure (mkName "aName")
instance Arbitrary TyLit where arbitrary = oneof [NumTyLit <$> arbitrary, StrTyLit <$> arbitraryConstructorName]
#if MIN_VERSION_template_haskell(2,17,0)
#else
instance Arbitrary TyVarBndr where arbitrary = oneof [PlainTV <$> arbitraryTypeVariableName, KindedTV <$> arbitraryTypeVariableName <*> arbitraryKind]
#endif

instance Arbitrary URIAuth where
    arbitrary =
      URIAuth <$> pure ""
              <*> genRegName
              <*> pure ""
        where
          genRegName = do
            domainName <- elements ["noomii", "google", "yahoo"]
            return $ mconcat ["www.", domainName, ".com"]

arbitraryKind :: Gen Kind
arbitraryKind = oneof [pure StarT {-, finish me -}]

arbitraryConstructorName :: Gen String
arbitraryConstructorName = pure "AConstructor"

arbitraryTypeVariableName :: Gen Name
arbitraryTypeVariableName = pure (mkName "aTyVarName")

-- s = $(location >>= \Loc{loc_module=m, loc_start=(sl,sc), loc_end=(el,ec)} -> lift (m <> ":" <> show sl))
#endif

#if 0
deriving instance Serialize AnnTarget
deriving instance Serialize Bang
deriving instance Serialize Body
deriving instance Serialize Callconv
deriving instance Serialize Clause
deriving instance Serialize Con
deriving instance Serialize Dec
deriving instance Serialize DerivClause
deriving instance Serialize DerivStrategy
deriving instance Serialize Exp
deriving instance Serialize FamilyResultSig
deriving instance Serialize Fixity
deriving instance Serialize FixityDirection
deriving instance Serialize Foreign
deriving instance Serialize FunDep
deriving instance Serialize Guard
deriving instance Serialize InjectivityAnn
deriving instance Serialize Inline
deriving instance Serialize Lit
deriving instance Serialize Match
deriving instance Serialize ModName
deriving instance Serialize Name
deriving instance Serialize NameFlavour
deriving instance Serialize NameSpace
deriving instance Serialize OccName
deriving instance Serialize Overlap
deriving instance Serialize Pat
deriving instance Serialize PatSynArgs
deriving instance Serialize PatSynDir
deriving instance Serialize Phases
deriving instance Serialize PkgName
deriving instance Serialize Pragma
deriving instance Serialize Range
deriving instance Serialize Role
deriving instance Serialize RuleBndr
deriving instance Serialize RuleMatch
deriving instance Serialize Safety
deriving instance Serialize SourceStrictness
deriving instance Serialize SourceUnpackedness
deriving instance Serialize Stmt
deriving instance Serialize TyLit
deriving instance Serialize Type
deriving instance Serialize TypeFamilyHead
deriving instance Serialize TySynEqn
#if MIN_VERSION_template_haskell(2,17,0)
#else
deriving instance Serialize TyVarBndr
#endif

#if 0
deriving instance NFData AnnTarget
deriving instance NFData Bang
deriving instance NFData Body
deriving instance NFData Callconv
deriving instance NFData Clause
deriving instance NFData Con
deriving instance NFData Dec
deriving instance NFData DerivClause
deriving instance NFData DerivStrategy
deriving instance NFData Exp
deriving instance NFData FamilyResultSig
deriving instance NFData Fixity
deriving instance NFData FixityDirection
deriving instance NFData Foreign
deriving instance NFData FunDep
deriving instance NFData Guard
deriving instance NFData InjectivityAnn
deriving instance NFData Inline
deriving instance NFData Lit
deriving instance NFData Match
deriving instance NFData ModName
deriving instance NFData Name
deriving instance NFData NameFlavour
deriving instance NFData NameSpace
deriving instance NFData OccName
deriving instance NFData Overlap
deriving instance NFData Pat
deriving instance NFData PatSynArgs
deriving instance NFData PatSynDir
deriving instance NFData Phases
deriving instance NFData PkgName
deriving instance NFData Pragma
deriving instance NFData Range
deriving instance NFData Role
deriving instance NFData RuleBndr
deriving instance NFData RuleMatch
deriving instance NFData Safety
deriving instance NFData TH.SourceStrictness
deriving instance NFData TH.SourceUnpackedness
deriving instance NFData Stmt
deriving instance NFData TyLit
deriving instance NFData Type
deriving instance NFData TypeFamilyHead
deriving instance NFData TySynEqn
#if MIN_VERSION_template_haskell(2,17,0)
#else
deriving instance NFData TyVarBndr
#endif
#endif

instance SafeCopy AnnTarget where version = 1
instance SafeCopy Bang where version = 1
instance SafeCopy Body where version = 1
instance SafeCopy Callconv where version = 1
instance SafeCopy Clause where version = 1
instance SafeCopy Con where version = 1
instance SafeCopy Dec where version = 1
instance SafeCopy DerivClause where version = 1
instance SafeCopy DerivStrategy where version = 1
instance SafeCopy Exp where version = 1
instance SafeCopy FamilyResultSig where version = 1
instance SafeCopy Fixity where version = 1
instance SafeCopy FixityDirection where version = 1
instance SafeCopy Foreign where version = 1
instance SafeCopy FunDep where version = 1
instance SafeCopy Guard where version = 1
instance SafeCopy InjectivityAnn where version = 1
instance SafeCopy Inline where version = 1
instance SafeCopy Lit where version = 1
instance SafeCopy Match where version = 1
-- instance SafeCopy ModName where version = 1
-- instance SafeCopy Name where version = 1
-- instance SafeCopy NameFlavour where version = 1
-- instance SafeCopy NameSpace where version = 1
-- instance SafeCopy OccName where version = 1
instance SafeCopy Overlap where version = 1
instance SafeCopy Pat where version = 1
instance SafeCopy PatSynArgs where version = 1
instance SafeCopy PatSynDir where version = 1
instance SafeCopy Phases where version = 1
-- instance SafeCopy PkgName where version = 1
instance SafeCopy Pragma where version = 1
instance SafeCopy Range where version = 1
instance SafeCopy Role where version = 1
instance SafeCopy RuleBndr where version = 1
instance SafeCopy RuleMatch where version = 1
instance SafeCopy Safety where version = 1
instance SafeCopy SourceStrictness where version = 1
instance SafeCopy SourceUnpackedness where version = 1
instance SafeCopy Stmt where version = 1
instance SafeCopy TyLit where version = 1
instance SafeCopy Type where version = 1
instance SafeCopy TypeFamilyHead where version = 1
instance SafeCopy TySynEqn where version = 1
#if MIN_VERSION_template_haskell(2,17,0)
instance SafeCopy (TyVarBndr flag) where version = 1
#else
instance SafeCopy TyVarBndr where version = 1
#endif

#if MIN_VERSION_template_haskell(2,16,0)
deriving instance Serialize Bytes
instance SafeCopy Bytes where version = 1
#if 0
deriving instance NFData Bytes
#endif
#endif
#endif
