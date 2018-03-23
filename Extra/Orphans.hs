{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Extra.Orphans where

import qualified Data.Graph.Inductive as G
import Data.Proxy (Proxy(Proxy))
import Data.SafeCopy (base, contain, SafeCopy(errorTypeName, getCopy, kind, putCopy, version))
import Data.Serialize (label, Serialize(..))
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Typeable (Typeable)
import Data.Time (UTCTime(..), Day(ModifiedJulianDay), toModifiedJulianDay, DiffTime)
import Data.UserId (UserId(..))
import Instances.TH.Lift ()
import Language.Haskell.TH (Loc(..))
import Language.Haskell.TH.Lift (deriveLift)

instance Typeable t => SafeCopy (Proxy t) where
      putCopy Proxy = contain (do { return () })
      getCopy = contain (label "Data.Proxy.Proxy:" (pure Proxy))
      version = 0
      kind = base
      errorTypeName _ = "Data.Proxy.Proxy"

instance Serialize UTCTime where
    get = uncurry UTCTime <$> get
    put (UTCTime day time) = put (day, time)

instance Serialize Day where
    get = ModifiedJulianDay <$> get
    put = put . toModifiedJulianDay

instance Serialize DiffTime where
    get = fromRational <$> get
    put = put . toRational

instance Serialize T.Text where
    put = put . TE.encodeUtf8
    get = TE.decodeUtf8 <$> get

instance Serialize LT.Text where
    put = put . TLE.encodeUtf8
    get = TLE.decodeUtf8 <$> get

deriving instance Serialize Loc

$(deriveLift ''UserId)

$(deriveLift ''G.Gr)
$(deriveLift ''G.NodeMap)
