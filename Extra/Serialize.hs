-- | A Serialize instance based on safecopy.  This means that migrations
-- will be performed upon deserialization, which can be nice.

{-# LANGUAGE CPP, TemplateHaskell #-}

module Extra.Serialize
    ( deriveSerialize
    ) where

import Data.SafeCopy (safeGet, safePut)
import Data.Serialize (Serialize(..))
-- import Data.THUnify.SerializeViaSafeCopy
import Language.Haskell.TH (Dec, TypeQ, Q)

-- | It turns out that this is a fortuitous choice for any type with a
-- SafeCopy instance, because it means values will be migrated as necessary
-- whenever they are decoded - even in the local storage of a web browser.
-- Thus, zero downtime upgrades!
deriveSerialize :: TypeQ -> Q [Dec]
deriveSerialize typ =
    [d|instance {-SafeCopy $typ =>-} Serialize $typ where
          get = safeGet
          put = safePut|]
