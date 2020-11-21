-- | This module exports a template haskell function to create
-- Serialize instances based on the SafeCopy instance, and an
-- alternative decode function that puts the decode type in the error
-- message.  It also re-exports all other Data.Serialize symbols

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Extra.SerializeDebug
    ( module Extra.Serialize
    , Debug
    , DecodeError(..)
    , HasDecodeError
    , fromDecodeError
    , deserializePrism
    , serializeGetter
    , decode
    , decode'
    , decodeM
    , decodeM'
    ) where

import Control.Exception (ErrorCall(..), evaluate, )
import Control.Lens (Getter, _Left, over, Prism', prism, re)
import Control.Monad.Catch (catch, MonadCatch)
import Control.Monad.Except (MonadError, throwError)
import Data.ByteString as B (ByteString, null)
#ifndef OMIT_DATA_INSTANCES
import Data.Data (Data)
#endif
import Data.Data (Proxy(Proxy), Typeable, typeRep)
import Data.SafeCopy (base, SafeCopy, safeGet, safePut)
import Data.Semigroup (Semigroup((<>)))
import Data.Serialize hiding (decode)
import qualified Data.Serialize as Serialize (decode, encode)
import Data.Text as T hiding (concat, intercalate)
import Data.Text.Lazy as LT hiding (concat, intercalate)
import Data.Text.Encoding as TE
import Data.Text.Lazy.Encoding as TLE
import Data.Time (UTCTime(..), Day(ModifiedJulianDay), toModifiedJulianDay, DiffTime)
import Data.UUID.Orphans ()
import Data.UUID (UUID)
import Data.UUID.Orphans ()
import Extra.Orphans ()
import Extra.Serialize hiding (decode, decode', decodeM, decodeM', deserializePrism, serializeGetter)
import Extra.Time (Zulu(..))
import Language.Haskell.TH (Dec, Loc, TypeQ, Q)
import Network.URI (URI(..), URIAuth(..))
import System.IO.Unsafe (unsafePerformIO)

type Debug a = (Typeable a, Show a)

-- | Decode a value from a strict ByteString, reconstructing the original
-- structure.  Unlike Data.Serialize.decode, this function only succeeds
-- if all the input is consumed.
decode :: forall a. (Serialize a, Debug a) => ByteString -> Either String a
decode b =
  case runGetState get b 0 of
    Left s -> Left s
    Right (a, remaining) | B.null remaining -> Right a
    Right (a, remaining) -> Left ("decode " <> show b <> " :: " <> show (typeRep (Proxy :: Proxy a)) <> " failed to consume " <> show remaining)

-- | Monadic version of decode.
decodeM ::
  forall a e m. (Serialize a, Debug a, HasDecodeError e, MonadError e m)
  => ByteString
  -> m a
decodeM bs =
  case decode bs of
    Left s -> throwError (fromDecodeError (DecodeError bs (fakeTypeRep (Proxy @a)) s))
    Right a -> return a

-- | Like 'decodeM', but also catches any ErrorCall thrown and lifts
-- it into the MonadError instance.  I'm not sure whether this can
-- actually happen.  What I'm seeing is probably an error call from
-- outside the serialize package, in which case this (and decode') are
-- pointless.
decodeM' ::
  forall e m a. (Serialize a, Debug a, Member DecodeError e, MonadError (OneOf e) m, MonadCatch m)
  => ByteString
  -> m a
decodeM' bs = go `catch` handle
  where
    go = case decode bs of
           Left s -> throwError (fromDecodeError (DecodeError bs (fakeTypeRep (Proxy @a)) s))
           Right a -> return a
    handle :: ErrorCall -> m a
    handle (ErrorCall s) = throwError $ fromDecodeError $ DecodeError bs (fakeTypeRep (Proxy @a)) s

-- | Version of decode that catches any thrown ErrorCall and modifies
-- its message.
decode' :: forall a. (Serialize a, Debug a) => ByteString -> Either String a
decode' b =
  unsafePerformIO (evaluate (decode b :: Either String a) `catch` handle)
  where
    handle :: ErrorCall -> IO (Either String a)
    handle e = return $ Left (show e)

-- | Serialize/deserialize prism.
deserializePrism :: forall a. (Serialize a, Debug a) => Prism' ByteString a
deserializePrism = prism encode (\s -> either (\_ -> Left s) Right (decode s :: Either String a))

-- | Inverting a prism turns it into a getter.
serializeGetter :: forall a. (Serialize a, Debug a) => Getter a ByteString
serializeGetter = re deserializePrism
