{-# LANGUAGE FlexibleInstances, StandaloneDeriving, TemplateHaskell, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Extra.EnvPath
    ( EnvRoot(..), rootPath
    , EnvPath(..), envRoot, envPath
    , outsidePath
    , appendPath
    , rootEnvPath
    , HasEnvRoot(envRootLens)
    ) where

import Control.Lens (Lens', makeLenses, over)
import Extra.Pretty (PP(PP))
import Text.PrettyPrint.HughesPJClass (text)
#if 0
import Distribution.Pretty (Pretty(pretty))
#else
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint))
#endif

-- |The root directory of an OS image.
data EnvRoot = EnvRoot { _rootPath :: FilePath } deriving (Ord, Eq, Read, Show)

-- |A directory inside of an OS image.
data EnvPath = EnvPath { _envRoot :: EnvRoot
                       , _envPath :: FilePath
                       } deriving (Ord, Eq, Read, Show)

$(makeLenses ''EnvRoot)
$(makeLenses ''EnvPath)

outsidePath :: EnvPath -> FilePath
outsidePath path = _rootPath (_envRoot path) ++ _envPath path

appendPath :: FilePath -> EnvPath -> EnvPath
appendPath suff path = over envPath (++ suff) path

rootEnvPath :: FilePath -> EnvPath
rootEnvPath s = EnvPath { _envRoot = EnvRoot "", _envPath = s }

instance Pretty (PP EnvRoot) where
    pPrint (PP x) = text (_rootPath x)

-- | Class used to access an EnvRoot in a value, typically in a reader
-- monad.
class HasEnvRoot r where envRootLens :: Lens' r EnvRoot
instance HasEnvRoot EnvRoot where envRootLens = id

