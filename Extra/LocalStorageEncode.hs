{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Extra.LocalStorageEncode (encode, decode, decodeM, encodeS, decodeS, urlEncode, urlDecode) where

import Control.Monad.Except
import Extra.Serialize as Serialize (DecodeError(..), fakeTypeRep, Serialize)
import qualified Extra.Serialize as Serialize (decodeM, encode)
import Data.Text as Text (Text, pack, unpack)
import qualified Data.Text.Encoding as Text (encodeUtf8, decodeUtf8)
import Data.Typeable (Proxy(Proxy), Typeable)
import qualified Data.ByteString.Base64 as Base64 (encode, decode)
import Extra.Errors (catchMember, runNullExcept, Member, OneOf(..), throwMember)
import Network.URI (escapeURIString, isUnreserved, unEscapeString)

encode :: Serialize a => a -> Text
encode = Text.decodeUtf8 . Base64.encode . Serialize.encode

-- We can ignore the possibility of Left (OneOf '[])
decode :: forall a. (Serialize a, Typeable a) => Text -> Either DecodeError a
decode t = runNullExcept $
  catchMember
    (withExceptT @_ @(OneOf '[DecodeError]) @(OneOf '[]))
    (decodeM t)
    (\(e :: DecodeError) -> return (Left e))

encodeS :: Serialize a => a -> String
encodeS =  Text.unpack . encode

decodeS :: (Serialize a, Typeable a) => String -> Either DecodeError a
decodeS = decode . Text.pack

decodeM :: forall a e m. (Serialize a, Typeable a, Member DecodeError e, MonadError (OneOf e) m) => Text -> m a
#if 0
decodeM = either fail return . join . fmap Serialize.decode . Base64.decode . Text.encodeUtf8
#else
decodeM t =
  case Base64.decode (Text.encodeUtf8 t) of
    Left s -> throwMember (DecodeError (Text.encodeUtf8 t) (fakeTypeRep (Proxy :: Proxy a)) s)
    Right b -> Serialize.decodeM b
#endif

urlEncode :: Serialize a => a -> String
urlEncode = escape . encodeS
  where escape = escapeURIString isUnreserved

urlDecode :: (Serialize a, Typeable a) => String -> Either DecodeError a
urlDecode = decodeS . unEscapeString
