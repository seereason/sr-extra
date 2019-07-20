-- | Copied from packman.  Tried to use the library but got a compile error:
--
-- <no location info>: error:
--     <command line>: can't load .so/.DLL for: /usr/lib/haskell-packages/ghc/lib/x86_64-linux-ghc-8.4.3/libHSpackman-0.5.0-Fv7reuyLHo03M7x4FerNmn-ghc8.4.3.so (/usr/lib/haskell-packages/ghc/lib/x86_64-linux-ghc-8.4.3/libHSpackman-0.5.0-Fv7reuyLHo03M7x4FerNmn-ghc8.4.3.so: undefined symbol: mblock_address_space)

{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable, DeriveGeneric, TemplateHaskell #-}

module Extra.FP
    ( FP(..)
    , matches
    , toFP
    , typeFP
    , typeRepFP
    ) where


--import Data.Binary (Binary(..), Get)
import Data.Data (Data, Proxy, typeRep)
import Data.SafeCopy (base, deriveSafeCopy)
import Extra.Serialize (Serialize)
import Data.Typeable (Typeable, typeOf)
import Data.Typeable (typeRepFingerprint)
import Data.Word (Word64)
import qualified GHC.Fingerprint
import GHC.Generics (Generic)

------------------------------------------------------------------
-- $ComparingTypes
-----------------------------------------------
-- Helper functions to compare types at runtime:
--   We use type "fingerprints" defined in 'GHC.Fingerprint.Type'

-- This should ensure (as of GHC.7.8) that types with the same name
-- but different definition get different hashes.  (however, we also
-- require the executable to be exactly the same, so this is not
-- strictly necessary anyway).

-- Typeable context for dynamic type checks. 

-- | The module uses a custom GHC fingerprint type with its two Word64
--   fields, to be able to /read/ fingerprints
data FP = FP Word64 Word64 deriving (Read, Show, Eq, Data, Typeable, Ord, Generic, Serialize)

-- | checks whether the type of the given expression matches the given Fingerprint
matches :: Typeable a => a -> FP -> Bool
matches x (FP c1 c2) = f1 == c1 && f2 == c2
  where  (GHC.Fingerprint.Fingerprint f1 f2) = typeRepFingerprint (typeOf x)

-- | creates an 'FP' from a GHC 'Fingerprint'
toFP :: GHC.Fingerprint.Fingerprint -> FP
toFP (GHC.Fingerprint.Fingerprint f1 f2) = FP f1 f2

-- | returns the type fingerprint of an expression
typeFP :: Typeable a => a -> FP
typeFP = toFP . typeRepFingerprint . typeOf

typeRepFP :: Typeable a => Proxy a -> FP
typeRepFP p = toFP (typeRepFingerprint (typeRep p))

{-
-- | Binary instance for fingerprint data (encoding TypeRep and
--   executable in binary-encoded @Serialized a@)
instance Binary FP where
  put (FP f1 f2) = do put f1
                      put f2
  get            = do f1 <- get :: Get Word64
                      f2 <- get :: Get Word64
                      return (FP f1 f2)
-}

$(deriveSafeCopy 1 'base ''FP)
