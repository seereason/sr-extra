-- | A Serialize instance based on safecopy.  This means that migrations
-- will be performed upon deserialization, which can be nice.

{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Extra.Serialize
    ( deserializePrism
    , serializeGetter
    , DecodeError(..)
    , HasDecodeError(fromDecodeError)
    , Serialize(..)
    , Serialize.encode
    , deriveSerializeViaSafeCopy
    , decodeM
    , decode
    , decode'
    ) where

import Control.Exception (ErrorCall(ErrorCall), evaluate, )
import Control.Lens (Getter, _Left, over, Prism', prism, re)
import Control.Monad.Catch (catch, MonadCatch, try)
import Control.Monad.Except (MonadError, throwError)
import Data.ByteString (ByteString)
import Data.Data (Proxy(Proxy), Typeable, typeRep)
import Data.SafeCopy (safeGet, safePut)
import Data.Serialize (Serialize)
import Data.Serialize (Serialize(..))
import qualified Data.Serialize as Serialize
import Language.Haskell.TH (Dec, TypeQ, Q)
import System.IO.Unsafe (unsafePerformIO)

-- | Serialize/deserialize prism.
deserializePrism :: forall a. Serialize a => Prism' ByteString a
deserializePrism = prism Serialize.encode (\s -> either (\_ -> Left s) Right (Serialize.decode s :: Either String a))

-- | Inverting a prism turns it into a getter.
serializeGetter :: forall a. Serialize a => Getter a ByteString
serializeGetter = re deserializePrism

-- | It turns out that this is a fortuitous choice for any type with a
-- SafeCopy instance, because it means values will be migrated as necessary
-- whenever they are decoded - even in the local storage of a web browser.
-- Thus, zero downtime upgrades!
deriveSerializeViaSafeCopy :: TypeQ -> Q [Dec]
deriveSerializeViaSafeCopy typ =
    [d|instance {-SafeCopy $typ =>-} Serialize $typ where
          get = safeGet
          put = safePut|]

data DecodeError =
    DecodeError String
  | ErrorCall' ErrorCall

class HasDecodeError e where fromDecodeError :: DecodeError -> e
instance HasDecodeError DecodeError where fromDecodeError = id

-- | Use this for decoding exclusively.  Remove build dependencies on
-- cereal to avoid stray use of its decode.
decodeM ::
  forall e m a. (Serialize a, Typeable a, HasDecodeError e, MonadError e m, MonadCatch m)
  => ByteString
  -> m a
decodeM b = go `catch` handle
  where
    go = case Serialize.decode b of
           Left s -> throwError (fromDecodeError (DecodeError (annotate s)))
           Right a -> return a
    handle :: ErrorCall -> m a
    handle (ErrorCall s) = throwError $ fromDecodeError $ ErrorCall' $ ErrorCall $ annotate s
    annotate :: String -> String
    annotate s = s <> " (decoding " <> show (typeRep (Proxy :: Proxy a)) <> ")"

-- | Modify the message returned on decode failure
decode :: forall a. (Serialize a, Typeable a) => ByteString -> Either String a
decode b =
  over _Left annotate (decode b :: Either String a)
  where
    annotate :: String -> String
    annotate s = s <> " (decoding " <> show (typeRep (Proxy :: Proxy a)) <> ")"

-- | Modify the message returned on decode failure
decode' :: forall a. (Serialize a, Typeable a) => ByteString -> Either String a
decode' b =
  unsafePerformIO (evaluate (decode b :: Either String a) `catch` handle)
  where
    handle :: ErrorCall -> IO (Either String a)
    handle (ErrorCall s) = return $ Left $ annotate s
    annotate :: String -> String
    annotate s = s <> " (decoding " <> show (typeRep (Proxy :: Proxy a)) <> ")"
