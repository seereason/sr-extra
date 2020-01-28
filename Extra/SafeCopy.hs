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
    , HasDecodeError(fromDecodeError)
    , decode
    , encode
    , decode'
    , decodeM
    , decodeM'
    ) where

import Control.Exception (ErrorCall(..), evaluate, )
import Control.Lens (Getter, Prism', prism, re)
import Control.Monad.Catch (catch, MonadCatch)
import Control.Monad.Except (MonadError, throwError)
import Data.ByteString as B (ByteString, null)
import Data.Data (Proxy(Proxy), Typeable)
import Data.SafeCopy (base, SafeCopy, safeGet, safePut)
import Data.Serialize hiding (decode, encode)
import Data.UUID.Orphans ()
import Extra.Orphans ()
import Extra.Serialize (DecodeError(..), fakeTypeRep, HasDecodeError(fromDecodeError))
import System.IO.Unsafe (unsafePerformIO)

encode :: SafeCopy a => a -> ByteString
encode = runPut . safePut

decode :: forall a. (SafeCopy a) => ByteString -> Either String a
decode b = case runGetState safeGet b 0 of
             Left s -> Left s
             Right (a, remaining) | B.null remaining -> Right a
             Right (a, remaining) -> Left ("decode " <> show b <> " failed to consume " <> show remaining)

-- | Monadic version of decode.
decodeM ::
  forall a e m. (SafeCopy a, Typeable a, HasDecodeError e, MonadError e m)
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
  forall e m a. (SafeCopy a, Typeable a, HasDecodeError e, MonadError e m, MonadCatch m)
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
decode' :: forall a. (SafeCopy a) => ByteString -> Either String a
decode' b =
  unsafePerformIO (evaluate (decode b :: Either String a) `catch` handle)
  where
    handle :: ErrorCall -> IO (Either String a)
    handle e = return $ Left (show e)

-- | Serialize/deserialize prism.
deserializePrism :: forall a. (SafeCopy a) => Prism' ByteString a
deserializePrism = prism encode (\s -> either (\_ -> Left s) Right (decode s :: Either String a))

-- | Inverting a prism turns it into a getter.
serializeGetter :: forall a. (SafeCopy a) => Getter a ByteString
serializeGetter = re deserializePrism
