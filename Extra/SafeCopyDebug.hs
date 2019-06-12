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

module Extra.SafeCopyDebug
    ( module Extra.SafeCopy
    , Debug
    , DecodeError(..)
    , HasDecodeError(fromDecodeError)
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
import Data.Serialize hiding (decode, encode)
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
import Extra.SafeCopy hiding (decode, decode', decodeM, decodeM')
import Extra.SerializeDebug (Debug, DecodeError(..), HasDecodeError(..))
import Extra.Time (Zulu(..))
import Language.Haskell.TH (Dec, Loc, TypeQ, Q)
import Network.URI (URI(..), URIAuth(..))
import System.IO.Unsafe (unsafePerformIO)

decode :: forall a. (SafeCopy a, Debug a) => ByteString -> Either String a
decode b = case runGetState safeGet b 0 of
             Left s -> Left s
             Right (a, remaining) | B.null remaining -> Right a
             Right (a, remaining) -> Left ("decode " <> show b <> " :: " <> show (typeRep (Proxy :: Proxy a)) <> " failed to consume " <> show remaining)

-- | Monadic version of decode.
decodeM ::
  forall a e m. (SafeCopy a, Debug a, HasDecodeError e, MonadError e m)
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
  forall e m a. (SafeCopy a, Debug a, HasDecodeError e, MonadError e m, MonadCatch m)
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
decode' :: forall a. (SafeCopy a, Debug a) => ByteString -> Either String a
decode' b =
  unsafePerformIO (evaluate (decode b :: Either String a) `catch` handle)
  where
    handle :: ErrorCall -> IO (Either String a)
    handle e = return $ Left (show e)

-- | Serialize/deserialize prism.
deserializePrism :: forall a. (SafeCopy a, Debug a) => Prism' ByteString a
deserializePrism = prism encode (\s -> either (\_ -> Left s) Right (decode s :: Either String a))

-- | Inverting a prism turns it into a getter.
serializeGetter :: forall a. (SafeCopy a, Debug a) => Getter a ByteString
serializeGetter = re deserializePrism
