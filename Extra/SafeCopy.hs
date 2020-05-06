-- | This module exports a template haskell function to create
-- Serialize instances based on the SafeCopy instance, and an
-- alternative decode function that puts the decode type in the error
-- message.  It also re-exports all other Data.Serialize symbols

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Extra.SafeCopy
    ( module Data.SafeCopy
    , DecodeError(..)
    , encodeSafe
    , decodeAllSafe
    , decodeMSafe
    , decodeMSafe'
    , decodePrismSafe
    , encodeGetterSafe
    ) where

import Control.Exception (ErrorCall(..))
import Control.Lens (Getter, Prism', prism, re)
import Control.Monad.Catch (catch, MonadCatch)
import Control.Monad.Except (MonadError)
import Data.ByteString as B (ByteString, null)
import Data.Data (Proxy(Proxy), Typeable)
import Data.SafeCopy (base, SafeCopy, safeGet, safePut)
import Data.Serialize hiding (decode, encode)
import Data.UUID.Orphans ()
import Extra.Errors (Member, OneOf, throwMember)
import Extra.Orphans ()
import Extra.Serialize (DecodeError(..), fakeTypeRep)

encodeSafe :: SafeCopy a => a -> ByteString
encodeSafe = runPut . safePut

-- Version of decode that errors if all input is not consumed.
decodeAllSafe :: forall a. (SafeCopy a) => ByteString -> Either String a
decodeAllSafe b =
  case runGetState safeGet b 0 of
    Left s -> Left s
    Right (a, more) | B.null more -> Right a
    Right (_, more) -> Left ("decode " <> show b <> " failed to consume " <> show more)

-- | Monadic version of decode.
decodeMSafe ::
  forall a e m. (SafeCopy a, Typeable a, Member DecodeError e, MonadError (OneOf e) m)
  => ByteString
  -> m a
decodeMSafe bs =
  case decodeAllSafe bs of
    Left s -> throwMember (DecodeError bs (fakeTypeRep (Proxy @a)) s)
    Right a -> return a

-- | Like 'decodeM', but also catches any ErrorCall thrown and lifts
-- it into the MonadError instance.  I'm not sure whether this can
-- actually happen.  What I'm seeing is probably an error call from
-- outside the serialize package, in which case this (and decode') are
-- pointless.
decodeMSafe' ::
  forall e m a. (SafeCopy a, Typeable a, Member DecodeError e, MonadError (OneOf e) m, MonadCatch m)
  => ByteString
  -> m a
decodeMSafe' bs = go `catch` handle
  where
    go = case decodeAllSafe bs of
           Left s -> throwMember (DecodeError bs (fakeTypeRep (Proxy @a)) s)
           Right a -> return a
    handle :: ErrorCall -> m a
    handle (ErrorCall s) = throwMember (DecodeError bs (fakeTypeRep (Proxy @a)) s)

-- | Serialize/deserialize prism.
decodePrismSafe :: forall a. (SafeCopy a) => Prism' ByteString a
decodePrismSafe = prism encodeSafe (\s -> either (\_ -> Left s) Right (decodeAllSafe s :: Either String a))

-- | Inverting a prism turns it into a getter.
encodeGetterSafe :: forall a. (SafeCopy a) => Getter a ByteString
encodeGetterSafe = re decodePrismSafe
