-- | This module exports a template haskell function to create
-- Serialize instances based on the SafeCopy instance, and an
-- alternative decode function that puts the decode type in the error
-- message.  It also re-exports all other Data.Serialize symbols

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Extra.Serialize
    ( DecodeError(..)
    , module Data.Serialize
    , deriveSerializeViaSafeCopy
    , decodeAll
    , decode'
    , decodeM
    , decodeM'
    , FakeTypeRep(..)
    , fakeTypeRep
    , decodePrism
    , encodeGetter
    ) where

import Control.Exception (ErrorCall(..), evaluate, )
import Control.Lens (Getter, Prism', prism, re)
import Control.Monad.Catch (catch, MonadCatch)
import Control.Monad.Except (MonadError)
import Data.ByteString as B (ByteString, null)
#ifndef OMIT_DATA_INSTANCES
import Data.Data (Data)
#endif
import Data.Data (Proxy(Proxy))
import Data.SafeCopy (SafeCopy(..), safeGet, safePut)
import Data.Serialize
import Data.Text as T hiding (concat, intercalate)
import Data.Text.Lazy as LT hiding (concat, intercalate)
import Data.Text.Encoding as TE
import Data.Text.Lazy.Encoding as TLE
import Data.Time (UTCTime(..), Day(ModifiedJulianDay), toModifiedJulianDay, DiffTime)
import Data.Typeable (Typeable, typeRep)
import Data.UUID.Orphans ()
import Data.UUID (UUID)
import Data.UUID.Orphans ()
import Extra.Errors (Member, OneOf, throwMember)
import Extra.Orphans ()
import Extra.Time (Zulu(..))
import GHC.Generics (Generic)
import Language.Haskell.TH (Dec, Loc, TypeQ, Q)
import Network.URI (URI(..), URIAuth(..))
import System.IO.Unsafe (unsafePerformIO)

#if 0
-- We can't make a Data instance for TypeRep because part of it is in
-- Data.Typeable.Internal, a hidden module in base.
instance SafeCopy TypeRep
deriving instance Data TypeRep
data DecodeError = DecodeError ByteString TypeRep String deriving (Generic, Eq, Ord, Typeable)
#else
newtype FakeTypeRep = FakeTypeRep String deriving (Generic, Eq, Ord, Serialize)
instance SafeCopy FakeTypeRep
fakeTypeRep :: forall a. Typeable a => Proxy a -> FakeTypeRep
fakeTypeRep a = FakeTypeRep (show (typeRep a))

data DecodeError = DecodeError ByteString FakeTypeRep String deriving (Generic, Eq, Ord, Typeable)
#endif

instance Serialize DecodeError where get = safeGet; put = safePut

-- | Decode a value from a strict ByteString, reconstructing the original
-- structure.  Unlike Data.Serialize.decode, this function only succeeds
-- if all the input is consumed.  Not sure if this is in use anywhere.
--
--   > Extra.Serialize.decode (encode 'x' <> encode 'y') :: Either String Char
--   Left "decode \"xy\" failed to consume \"y\""
--   > Data.Serialize.decode (encode 'x' <> encode 'y') :: Either String Char
--   Right 'x'
decodeAll :: forall a. Serialize a => ByteString -> Either String a
decodeAll b =
  case runGetState get b 0 of
    Left s -> Left s
    Right (a, more) | B.null more -> Right a
    Right (_, more) -> Left ("decode " <> show b <> " failed to consume " <> show more)

-- | A Serialize instance based on safecopy.  This means that
-- migrations will be performed upon deserialization, which is handy
-- if the value is stored in the browser's local storage.  Thus, zero
-- downtime upgrades!
deriveSerializeViaSafeCopy :: TypeQ -> Q [Dec]
deriveSerializeViaSafeCopy typ =
    [d|instance {-SafeCopy $typ =>-} Serialize $typ where
          get = safeGet
          put = safePut|]

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

-- | Monadic version of decode.
decodeM :: forall a e m. (Serialize a, Typeable a, Member DecodeError e, MonadError (OneOf e) m)
  => ByteString
  -> m a
decodeM bs =
  case decode bs of
    Left s -> throwMember (DecodeError bs (fakeTypeRep (Proxy @a)) s)
    Right a -> return a

-- | Like 'decodeM', but also catches any ErrorCall thrown and lifts
-- it into the MonadError instance.  I'm not sure whether this can
-- actually happen.  What I'm seeing is probably an error call from
-- outside the serialize package, in which case this (and decode') are
-- pointless.
decodeM' ::
  forall e m a. (Serialize a, Typeable a, Member DecodeError e, MonadError (OneOf e) m, MonadCatch m)
  => ByteString
  -> m a
decodeM' bs = go `catch` handle
  where
    go = case decode bs of
           Left s -> throwMember (DecodeError bs (fakeTypeRep (Proxy @a)) s)
           Right a -> return a
    handle :: ErrorCall -> m a
    handle (ErrorCall s) = throwMember $ DecodeError bs (fakeTypeRep (Proxy @a)) ("ErrorCall: " <> s)

-- | Version of decode that catches any thrown ErrorCall and modifies
-- its message.
decode' :: forall a. (Serialize a) => ByteString -> Either String a
decode' b =
  unsafePerformIO (evaluate (decode b :: Either String a) `catch` handle)
  where
    handle :: ErrorCall -> IO (Either String a)
    handle e = return $ Left (show e)

decodePrism :: forall a. (Serialize a) => Prism' ByteString a
decodePrism = prism encode (\s -> either (\_ -> Left s) Right (decode s :: Either String a))

encodeGetter :: forall a. (Serialize a) => Getter a ByteString
encodeGetter = re decodePrism

instance SafeCopy DecodeError where version = 1

#ifndef OMIT_DATA_INSTANCES
deriving instance Data FakeTypeRep
deriving instance Data DecodeError
#endif

#ifndef OMIT_SHOW_INSTANCES
deriving instance Show FakeTypeRep
deriving instance Show DecodeError
#endif
