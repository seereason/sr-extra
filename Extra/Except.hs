{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -Wredundant-constraints -Wno-orphans #-}

module Extra.Except
    ( -- * Control.Exception extensions
      withException
    , displaySomeExceptionType
      -- * Control.Monad.Except extensions
    , tryError
    , withError
    , withError'
    , mapError
    , handleError
    , HasIOException(ioException)
    , NonIOException(NonIOException)
    , HasNonIOException(nonIOException)
    , splitException
    , HasErrorCall(..)
    , IOException'(..)
    , MonadUIO
    , lyftIO
#if !__GHCJS__
    -- , logIOError
#endif
    , module Control.Monad.Except
    ) where

import Control.Applicative
import Control.Exception hiding (catch) -- ({-evaluate,-} Exception, IOException, SomeException(..))
import Control.Lens (Prism', review)
import Control.Monad.Catch
import Control.Monad.Except (catchError, ExceptT, liftEither, MonadError, MonadPlus(mzero, mplus), runExceptT, throwError, withExceptT)
import Data.Monoid ((<>))
import Data.Serialize
import Data.Typeable (typeOf)
#if !__GHCJS__
--import Extra.Log (logException, Priority(ERROR))
#endif
import Foreign.C.Types (CInt(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOException(IOError), IOErrorType(..))
import UnexceptionalIO.Trans

deriving instance Generic CInt
deriving instance Serialize CInt
deriving instance Generic IOErrorType
deriving instance Serialize IOErrorType
deriving instance Generic IOException
instance Serialize IOException where
  put (IOError _handle a b c d e) = put a >> put b >> put c >> put d >> put e
  get = uncurry6 IOError (pure Nothing, get, get, get, get, get)
    where uncurry6 f (h, t, l, d, e, p) = f <$> h <*> t <*> l <*> d <*> e <*> p

-- | Apply a function to whatever @Exception@ type is inside a
-- @SomeException@:
--
-- >>> catch (readFile "/tmp/nonexistant") (withException (return . show . typeOf))
-- "IOException"
withException :: forall r. (forall e. Exception e => e -> r) -> SomeException -> r
withException f (SomeException e) = f e

-- | Use 'withException' to obtain the exception's type name (similar
-- to 'Control.Exception.displayException')
--
-- >>> catch (readFile "/tmp/nonexistant") (return . displaySomeExceptionType)
-- "IOException"
displaySomeExceptionType :: SomeException -> String
displaySomeExceptionType = withException (show . typeOf)

-- | MonadError analog to the 'try' function.
tryError :: MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)

-- | Absorb an ExceptT e' action into another MonadError instance.
withError :: MonadError e m => (e' -> e) -> ExceptT e' m a -> m a
withError f action = runExceptT (withExceptT f action) >>= liftEither

-- | Modify the value (but not the type) of an error
withError' :: MonadError e m => (e -> e) -> m a -> m a
withError' f action = tryError action >>= either (throwError . f) return
{-# DEPRECATED withError' "withError might to be able to do this job" #-}

handleError :: MonadError e m => (e -> m a) -> m a -> m a
handleError = flip catchError

-- | MonadError analogue of the 'mapExceptT' function.
mapError :: (MonadError e m, MonadError e' n) => (m (Either e a) -> n (Either e' b)) -> m a -> n b
mapError f action = f (tryError action) >>= liftEither

-- | In order to guarantee IOException is caught, do NOT create this
-- 'HasIOException' instance for IOException.
--
-- > instance HasIOException IOException where fromIOException = id
--
-- Because there is an @instance MonadError IOException IO@ in
-- @Control.Monad.Except@, "thrown" IOexceptions are not be caught by
-- 'runExceptT':
--
-- >>> runExceptT (liftIO (readFile "/etc/nonexistant") :: ExceptT IOException IO String)
-- *** Exception: /etc/nonexistant: openFile: does not exist (No such file or directory)
--
--  (*** means the exception reached the top level.)  However, if we
-- use 'liftIOError' to implement a version of 'readFile' that has a
-- 'MonadIOError' constraint:
--
-- >>> let readFile' path = liftIOError (readFile path)
-- >>> :type readFile'
-- readFile' :: MonadIOError e m => FilePath -> m String
--
-- and then create a suitable error type
--
-- >>> newtype Error = Error IOException deriving Show
-- >>> instance MonadIOError Error where liftIOError io = liftIO (try io) >>= either (throwError . fromIOException) return
--
-- Now the thrown 'IOException' will always be caught and lifted into
-- the 'MonadError':
--
-- >>> runExceptT (readFile' "/etc/nonexistant" :: ExceptT Error IO String)
-- Left (Error /etc/nonexistant: openFile: does not exist (No such file or directory))
class HasIOException e where ioException :: Prism' e IOException
instance HasIOException IOException where ioException = id

newtype IOException' = IOException' IOException
instance Show IOException' where show (IOException' e) = "(IOException' " <> show (show e) <> ")"

class HasErrorCall e where fromErrorCall :: ErrorCall -> e
instance HasErrorCall ErrorCall where fromErrorCall = id

#if !__GHCJS__
--logIOError :: (MonadIO m, MonadError e m) => m a -> m a
--logIOError = handleError (\e -> liftIO ($logException ERROR (pure e)) >> throwError e)
#endif

instance MonadCatch UIO where
  catch uio f = unsafeFromIO $ catch (run uio) (\e -> run (f e))
instance MonadThrow UIO where
  throwM = unsafeFromIO . throwM

instance MonadPlus UIO where
  mzero = unsafeFromIO mzero
  mplus a b = unsafeFromIO (mplus (run a) (run b))
instance Alternative UIO where
  empty = unsafeFromIO empty
  a <|> b = unsafeFromIO (run a <|> run b)

type MonadUIO = Unexceptional

lyftIO :: (Unexceptional m, MonadError e m, HasIOException e, HasNonIOException e) => IO a -> m a
lyftIO io = runExceptT (fromIO io) >>= either (throwError . splitException) return

-- | If 'fromIO' throws a SomeNonPseudoException, 'splitException'
-- decides whether it was an 'IOException' or something else, this
-- wrapper indicates it was something else.
newtype NonIOException = NonIOException SomeNonPseudoException deriving Show
class HasNonIOException e where nonIOException :: Prism' e NonIOException
instance HasNonIOException NonIOException where nonIOException = id

splitException :: (HasIOException e, HasNonIOException e) => SomeNonPseudoException -> e
splitException e =
  maybe (review nonIOException (NonIOException e)) (review ioException)
    (fromException (toException e) :: Maybe IOException)
