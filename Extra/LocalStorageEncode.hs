{-# LANGUAGE CPP, RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Extra.LocalStorageEncode (encode, decode, encodeS, decodeS, urlEncode, urlDecode) where

import Control.Monad.Except
import qualified Data.Serialize as Serialize (decode)
import Extra.Serialize as Serialize (DecodeError(..), fakeTypeRep, Serialize)
import qualified Extra.Serialize as Serialize (encode)
import Data.Text as Text (Text, pack, unpack)
import qualified Data.Text.Encoding as Text (encodeUtf8, decodeUtf8)
import Data.Typeable (Proxy(Proxy), Typeable)
import qualified Data.ByteString.Base64 as Base64 (encode, decode)
import Network.URI (escapeURIString, isUnreserved, unEscapeString)

encode :: Serialize a => a -> Text
encode = Text.decodeUtf8 . Base64.encode . Serialize.encode

decode :: forall a. (Serialize a, Typeable a) => Text -> Either DecodeError a
#if 0
decode = either fail return . join . fmap Serialize.decode . Base64.decode . Text.encodeUtf8
#else
decode t =
  case Base64.decode (Text.encodeUtf8 t) of
    Left s -> throwError (DecodeError (Text.encodeUtf8 t) (fakeTypeRep (Proxy :: Proxy a)) s)
    Right b ->
      case Serialize.decode b of
        Left s -> throwError (DecodeError (Text.encodeUtf8 t) (fakeTypeRep (Proxy :: Proxy a)) s)
        Right a -> return a
#endif

encodeS :: Serialize a => a -> String
encodeS =  Text.unpack . encode

decodeS :: (Serialize a, Typeable a) => String -> Either DecodeError a
decodeS = decode . Text.pack

urlEncode :: Serialize a => a -> String
urlEncode = escape . encodeS
  where escape = escapeURIString isUnreserved

urlDecode :: (Serialize a, Typeable a) => String -> Either DecodeError a
urlDecode = decodeS . unEscapeString
