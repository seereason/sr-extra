{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}

module Extra.Orphans2 where

import Data.Generics (TyCon, TypeRep, tyConFingerprint, tyConModule, tyConName, tyConPackage, splitTyConApp)
import Data.Int (Int32)
import Data.List (intercalate)
import Data.ListLike as LL hiding (show)
import Data.Map as Map (Map, toList)
import Data.SafeCopy (SafeCopy(..))
import Data.Set as Set (Set, toList)
#if !__GHCJS__
import Debug.Show (V(V))
#endif
import Language.Haskell.TH
import Language.Haskell.TH.PprLib (Doc, hcat, ptext, vcat)
import Language.Haskell.TH.Syntax (ModName, NameFlavour, OccName, PkgName)
import Prelude hiding (concat, foldl1)

instance Ppr (Type, Int32) where
  ppr (t, n) = pprPair (t, n)

instance Ppr Int32 where ppr = ptext . show

instance Ppr (Name, [Type]) where
    ppr (name, params) = ppr (foldl1 AppT (ConT name : params))

pprPair :: (Ppr a, Ppr b) => (a, b) -> Doc
pprPair (a, b) = hcat [ptext "(", ppr a, ptext ",", ppr b, ptext ")"]

pprList :: [Doc] -> Doc
pprList xs = hcat [ptext "[", hcat (intersperse (ptext ",") xs), ptext "]"]

-- deriving instance Data CmdSpec

#if 0
instance Arbitrary ReportImageID where arbitrary = ReportImageID <$> arbitrary
instance Arbitrary ReportElemID where arbitrary = ReportElemID <$> arbitrary
instance (Arbitrary v, Enum k, Ord k) => Arbitrary (Order k v) where
    arbitrary = sized $ \n -> do
      vs <- vectorOf n (arbitrary :: Gen v)
      ks <- shuffle (take n [toEnum 0..] :: [k])
      fromPairs <$> shuffle (zip ks vs)
#endif

instance Ppr Char where ppr = ptext . show
instance Ppr Float where ppr = ptext . show
-- instance Ppr ReportElemID where ppr = ptext . show
-- instance Ppr ReportImageID where ppr = ptext . show
instance (Ppr k, Ppr v) => Ppr (Map k v) where ppr = pprList . fmap pprPair . Map.toList
#if 0
instance (Ppr k, Ppr v) => Ppr (Order k v) where ppr = pprList . fmap pprPair . LL.toList . toPairs
#endif
instance Ppr (Int, Char) where ppr = ptext . show

instance Ppr Bool where
    ppr True = ptext "True"
    ppr False = ptext "False"

#if !__GHCJS__
instance Show (V TyCon) where
    show (V c) =
        "TyCon {" ++
        "module=" ++ show (tyConModule c) ++
        " name=" ++ show (tyConName c) ++
        " package=" ++ show (tyConPackage c) ++
        " fingerprint=" ++ show (tyConFingerprint c) ++ "}"

instance Show (V TypeRep) where
    show (V r) =
        case splitTyConApp r of
          (c, rs) -> "[" ++ intercalate "," (show (V c) : fmap (show . V) rs) ++ "]"
#endif

instance Ppr TypeRep where
  ppr = ptext . show

instance Ppr () where
    ppr () = ptext "()"

-- | 'Int' is the 'Data.Path.Index.ContainerKey' type for all lists, so
-- we need to make sure all the required instances exist.
instance Ppr Int where
    ppr = ptext . show

instance Ppr (Set Type, Set Type) where
    ppr (extra, missing) = vcat [ptext "extra:", ppr extra, ptext "missing:", ppr missing]

instance Ppr (Set Type) where
    ppr s = hcat [ptext "Set.fromList [", ppr (Set.toList s), ptext "]"]

instance SafeCopy OccName where version = 0
instance SafeCopy NameSpace where version = 0
instance SafeCopy PkgName where version = 0
instance SafeCopy ModName where version = 0
instance SafeCopy NameFlavour where version = 0
instance SafeCopy Name where version = 0
instance SafeCopy Loc where version = 1
