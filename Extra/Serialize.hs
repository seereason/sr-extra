-- | This module exports a template haskell function to create
-- Serialize instances based on the SafeCopy instance, and an
-- alternative decode function that puts the decode type in the error
-- message.  It also re-exports all other Data.Serialize symbols

{-# LANGUAGE CPP #-}
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
import Control.Monad.Catch (catch, MonadCatch, try)
import Control.Monad.Except (MonadError, throwError)
import Data.ByteString (ByteString)
import Data.Data (Data, Proxy(Proxy), Typeable, typeRep)
import Data.SafeCopy (base, deriveSafeCopy, safeGet, safePut)
import Data.Serialize hiding (decode)
import qualified Data.Serialize as Serialize (decode)
import Language.Haskell.TH (Dec, TypeQ, Q)
import System.IO.Unsafe (unsafePerformIO)

-- | Serialize/deserialize prism.
deserializePrism :: forall a. Serialize a => Prism' ByteString a
deserializePrism = prism encode (\s -> either (\_ -> Left s) Right (Serialize.decode s :: Either String a))

-- | Inverting a prism turns it into a getter.
serializeGetter :: forall a. Serialize a => Getter a ByteString
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

deriving instance Data ErrorCall

data DecodeError = DecodeError ByteString String deriving (Show, Data, Eq, Ord)

class HasDecodeError e where fromDecodeError :: DecodeError -> e
instance HasDecodeError DecodeError where fromDecodeError = id

-- | Monadic version of decode.
decodeM ::
  forall e m a. (Serialize a, Typeable a, HasDecodeError e, MonadError e m)
  => ByteString
  -> m a
decodeM b =
  case decode b of
    Left s -> throwError (fromDecodeError (DecodeError b s))
    Right a -> return a
  where
    annotate :: String -> String
    annotate s = s <> " (decoding " <> show (typeRep (Proxy :: Proxy a)) <> ")"

-- | Like 'decodeM', but also catches any ErrorCall thrown and lifts
-- it into the MonadError instance.  I'm not sure whether this can
-- actually happen.  What I'm seeing is probably an error call from
-- outside the serialize package, in which case this (and decode') are
-- pointless.
decodeM' ::
  forall e m a. (Serialize a, Typeable a, HasDecodeError e, MonadError e m, MonadCatch m)
  => ByteString
  -> m a
decodeM' b = go `catch` handle
  where
    go = case decode b of
           Left s -> throwError (fromDecodeError (DecodeError b s))
           Right a -> return a
    handle :: ErrorCall -> m a
    handle (ErrorCall s) = throwError $ fromDecodeError $ DecodeError b $ annotate s
    annotate :: String -> String
    annotate s = s <> " (decoding " <> show (typeRep (Proxy :: Proxy a)) <> ")"

-- | Like 'Serialize.decode' but annotates the error string with the
-- decode type, adds constraint @Typeable a@.
decode :: forall a. (Serialize a, Typeable a) => ByteString -> Either String a
decode b =
  over _Left annotate (decode b :: Either String a)
  where
    annotate :: String -> String
    annotate s = s <> " (decoding " <> show (typeRep (Proxy :: Proxy a)) <> ")"

-- | Version of decode that catches any thrown ErrorCall and modifies
-- its message.
decode' :: forall a. (Serialize a, Typeable a) => ByteString -> Either String a
decode' b =
  unsafePerformIO (evaluate (decode b :: Either String a) `catch` handle)
  where
    handle :: ErrorCall -> IO (Either String a)
    handle (ErrorCall s) = return $ Left $ annotate s
    annotate :: String -> String
    annotate s = s <> " (decoding " <> show (typeRep (Proxy :: Proxy a)) <> ")"

$(concat <$>
  sequence
  [ deriveSafeCopy 1 'base ''ErrorCall
  , deriveSafeCopy 1 'base ''DecodeError
  ])
