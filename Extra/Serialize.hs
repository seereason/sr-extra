-- | This module exports a template haskell function to create
-- Serialize instances based on the SafeCopy instance, and an
-- alternative decode function that puts the decode type in the error
-- message.  It also re-exports all other Data.Serialize symbols

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Extra.Serialize
    ( module Data.Serialize
    , deserializePrism
    , serializeGetter
    , DecodeError(..)
    , HasDecodeError(fromDecodeError)
    , deriveSerializeViaSafeCopy
    , decode
    , decode'
    , decodeM
    , decodeM'
    ) where

import Control.Exception (ErrorCall(..), evaluate, )
import Control.Lens (Getter, _Left, over, Prism', prism, re)
import Control.Monad.Catch (catch, MonadCatch)
import Control.Monad.Except (MonadError, throwError)
import Data.ByteString (ByteString)
#ifndef OMIT_DATA_INSTANCES
import Data.Data (Data)
#endif
import Data.Data (Proxy(Proxy), Typeable, typeRep)
import Data.SafeCopy (base, deriveSafeCopy, safeGet, safePut)
import Data.Serialize hiding (decode)
import qualified Data.Serialize as Serialize (decode)
import Data.Text as T hiding (concat, intercalate)
import Data.Text.Lazy as LT hiding (concat, intercalate)
import Data.Text.Encoding as TE
import Data.Text.Lazy.Encoding as TLE
import Data.Time (UTCTime(..), Day(ModifiedJulianDay), toModifiedJulianDay, DiffTime)
import Data.UUID.Orphans ()
import Data.UUID (UUID)
import Data.UUID.Orphans ()
import Extra.Orphans ()
import Extra.Time (Zulu(..))
import Language.Haskell.TH (Dec, Loc, TypeQ, Q)
import Network.URI (URI(..), URIAuth(..))
import System.IO.Unsafe (unsafePerformIO)

-- | Serialize/deserialize prism.
deserializePrism :: forall a. (Serialize a, Typeable a) => Prism' ByteString a
deserializePrism = prism encode (\s -> either (\_ -> Left s) Right (decode s :: Either String a))

-- | Inverting a prism turns it into a getter.
serializeGetter :: forall a. (Serialize a, Typeable a) => Getter a ByteString
serializeGetter = re deserializePrism

-- | A Serialize instance based on safecopy.  This means that
-- migrations will be performed upon deserialization, which is handy
-- if the value is stored in the browser's local storage.  Thus, zero
-- downtime upgrades!
deriveSerializeViaSafeCopy :: TypeQ -> Q [Dec]
deriveSerializeViaSafeCopy typ =
    [d|instance {-SafeCopy $typ =>-} Serialize $typ where
          get = safeGet
          put = safePut|]

data DecodeError = DecodeError ByteString String deriving (Eq, Ord)

class HasDecodeError e where fromDecodeError :: DecodeError -> e
instance HasDecodeError DecodeError where fromDecodeError = id

-- | Like 'Serialize.decode' but annotates the error string with the
-- decode type, adds constraint @Typeable a@.
decode :: forall a. (Serialize a, Typeable a) => ByteString -> Either String a
decode bs =
  over _Left annotate (Serialize.decode bs :: Either String a)
  where
    annotate :: String -> String
    annotate e = "error - (decode " ++ show bs ++ ") :: " ++ show (typeRep (Proxy :: Proxy a)) ++ " -> " ++ show e

-- | Monadic version of decode.
decodeM ::
  forall a e m. (Serialize a, Typeable a, HasDecodeError e, MonadError e m)
  => ByteString
  -> m a
decodeM bs =
  case decode bs of
    Left s -> throwError (fromDecodeError (DecodeError bs s))
    Right a -> return a

-- | Like 'decodeM', but also catches any ErrorCall thrown and lifts
-- it into the MonadError instance.  I'm not sure whether this can
-- actually happen.  What I'm seeing is probably an error call from
-- outside the serialize package, in which case this (and decode') are
-- pointless.
decodeM' ::
  forall e m a. (Serialize a, Typeable a, HasDecodeError e, MonadError e m, MonadCatch m)
  => ByteString
  -> m a
decodeM' bs = go `catch` handle
  where
    go = case decode bs of
           Left s -> throwError (fromDecodeError (DecodeError bs s))
           Right a -> return a
    handle :: ErrorCall -> m a
    handle (ErrorCall s) = throwError $ fromDecodeError $ DecodeError bs s

-- | Version of decode that catches any thrown ErrorCall and modifies
-- its message.
decode' :: forall a. (Serialize a, Typeable a) => ByteString -> Either String a
decode' b =
  unsafePerformIO (evaluate (decode b :: Either String a) `catch` handle)
  where
    handle :: ErrorCall -> IO (Either String a)
    handle e = return $ Left (show e)

$(concat <$>
  sequence
  [ deriveSafeCopy 1 'base ''ErrorCall
  , deriveSafeCopy 1 'base ''DecodeError
  ])

#ifndef OMIT_DATA_INSTANCES
deriving instance Data ErrorCall
deriving instance Data DecodeError
#endif

#ifndef OMIT_SHOW_INSTANCES
deriving instance Show DecodeError
#endif

-- * Orphan Serialize instances

instance Serialize T.Text where
    put = put . TE.encodeUtf8
    get = TE.decodeUtf8 <$> get

instance Serialize LT.Text where
    put = put . TLE.encodeUtf8
    get = TLE.decodeUtf8 <$> get

-- | This is private, we can't create a Generic instance for it.
instance Serialize DiffTime where
    get = fromRational <$> get
    put = put . toRational

instance Serialize UTCTime where
    get = uncurry UTCTime <$> get
    put (UTCTime day time) = put (day, time)

instance Serialize Day where
    get = ModifiedJulianDay <$> get
    put = put . toModifiedJulianDay

-- deriving instance Generic UUID deriving instance Serialize UUID Use
-- the SafeCopy methods to implement Serialize.  This is a pretty neat
-- trick, it automatically does SafeCopy migration on any deserialize
-- of a type with this implementation.
instance Serialize UUID where
    get = safeGet
    put = safePut

deriving instance Serialize Loc
deriving instance Serialize URI
deriving instance Serialize URIAuth
deriving instance Serialize Zulu
