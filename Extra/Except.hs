{-# LANGUAGE CPP #-}
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
    , HasNonIOException(nonIOException)
    , HasErrorCall(..)
    , IOException'(..)
    -- , HasSomeNonPseudoException(someNonPseudoException)
    , SomeNonPseudoException'
    -- , lyftIO
    -- , lyftIO'
    , lyftIO'
    , lyftIO
#if !__GHCJS__
    , logIOError
#endif
    , module Control.Monad.Except
    ) where

import Control.Applicative
import Control.Exception hiding (catch) -- ({-evaluate,-} Exception, IOException, SomeException(..))
import Control.Lens (iso, Prism', preview, prism, review, _Right)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Trans (MonadTrans(lift), liftIO)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Maybe (fromJust)
import Data.Serialize
import Data.Typeable (Typeable, typeOf)
import Extra.ErrorControl
#if !__GHCJS__
import Extra.Log (logException, Priority(ERROR))
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
logIOError :: (MonadIO m, MonadError e m) => m a -> m a
logIOError = handleError (\e -> liftIO ($logException ERROR (pure e)) >> throwError e)
#endif

instance MonadCatch UIO where
  catch uio f = unsafeFromIO $ catch (run uio) (\e -> run (f e))
instance MonadThrow UIO where
  throwM = unsafeFromIO . throwM

#if 0
instance Unexceptional m => Unexceptional (ServerPartT m) where
  lift = error "instance Unexceptional (ServerPartT m)"
#endif

instance MonadPlus UIO where
  mzero = unsafeFromIO mzero
  mplus a b = unsafeFromIO (mplus (run a) (run b))
instance Alternative UIO where
  empty = unsafeFromIO empty
  a <|> b = unsafeFromIO (run a <|> run b)

#if 0
-- | Like 'UnexceptionalIO.Trans.fromIO'', but lifts into any
-- 'MonadError' instance rather than only ExceptT.
lyftIO' ::
  (Unexceptional m, Exception e, MonadError e m)
  => (SomeNonPseudoException -> e)
  -> IO a
  -> m a
lyftIO' f io =
  runExceptT (fromIO' f io) >>= liftEither
  -- withError id (fromIO' (view someNonPseudoException) io)

-- | Like 'lyftIO' but gets the function argument from the type class
-- 'HasSomeNonPseudoException'.
lyftIO ::
  (Unexceptional m, Exception e, MonadError e m, HasSomeNonPseudoException e)
  => IO a
  -> m a
lyftIO = lyftIO' (review someNonPseudoException)
#endif

lyftIO' ::
  forall e m a. (Unexceptional m, HasNonIOException e, HasIOException e, Exception e)
  => IO a
  -> ExceptT e m a
lyftIO' io =
  controlError
    (fromIO io :: ExceptT SomeNonPseudoException m a)
    (\(e :: SomeNonPseudoException) -> (maybe (throwError (review nonIOException e)) (\e' -> throwError (review ioException e')) (preview ioException e)))

lyftIO ::
  (Unexceptional m, Exception e, MonadError e m, HasNonIOException e, HasIOException e)
  => IO a
  -> m a
lyftIO io = runExceptT (lyftIO' io) >>= either throwError return

class HasSomeNonPseudoException e where
  someNonPseudoException :: Prism' e SomeNonPseudoException
instance HasSomeNonPseudoException SomeNonPseudoException where
  someNonPseudoException = id

-- | This is an error that was caught by UnexceptionalIO and is not an
-- IO exception.  It is stored as a SomeNonPseudoException (or should
-- it be a SomeException?)
class HasNonIOException e where
  nonIOException :: Prism' e SomeNonPseudoException

newtype NonIOException e = NonIOException {unNonIOException :: e}

-- | A type we can derive from a 'SomeNonPseudoException' that has
-- IOException and NonIOException instances.
type SomeNonPseudoException' =
  Either (NonIOException SomeNonPseudoException) IOException

-- | An example of a type that splits a 'SomeNonPseudoException' into
-- an IOException and a 'NonIOException'.  It happens to be an Iso'.
instance HasNonIOException SomeNonPseudoException' where
  nonIOException :: Prism' SomeNonPseudoException' SomeNonPseudoException
  nonIOException = iso f g
    where
      f :: SomeNonPseudoException' -> SomeNonPseudoException
      f = either unNonIOException (fromJust . fromException . toException)
      g :: SomeNonPseudoException -> SomeNonPseudoException'
      g e = maybe (Left (NonIOException e)) Right (fromException (toException e))

-- The HasIOException instance is easy.
instance HasIOException SomeNonPseudoException' where
  ioException = _Right

instance HasIOException SomeNonPseudoException where
  ioException :: Prism' SomeNonPseudoException IOException
  ioException = prism f g
    where
      f :: IOException -> SomeNonPseudoException
      -- Because IOException is a non-pseudo exception this
      -- fromException will return a Just
      f = fromJust . fromException . toException
      g :: SomeNonPseudoException -> Either SomeNonPseudoException IOException
      g e = maybe (Left e) Right (fromException (toException e))

-- Now use ErrorControl to handle the NonIOException and leave the IOException.

instance Monad m => ErrorControl SomeNonPseudoException' (ExceptT SomeNonPseudoException' m) (ExceptT IOException m) where
  controlError :: ExceptT SomeNonPseudoException' m a -> (SomeNonPseudoException' -> ExceptT IOException m a) -> ExceptT IOException m a
  controlError ma f = mapExceptT (\ma' -> ma' >>= either (runExceptT . f) (pure . Right)) ma
  accept :: ExceptT IOException m a -> ExceptT SomeNonPseudoException' m a
  accept = withExceptT Right

-- fromException :: Exception e => SomeException -> Maybe e
-- toException :: Exception e => e -> SomeException

-- | Convert a SomeNonPseudoException into any error type with
-- both IOException and NonIOException instances.  This has a
-- problem when you try to use 'accept' to convert an e back into
-- a SomeNonPseudoException, so best not to use accept.
#if 1
instance (Monad m, HasIOException e, HasNonIOException e, {-Typeable e, Show e,-} Exception e)
    => ErrorControl SomeNonPseudoException (ExceptT SomeNonPseudoException m) (ExceptT e m) where
  controlError :: ExceptT SomeNonPseudoException m a -> (SomeNonPseudoException -> ExceptT e m a) -> ExceptT e m a
  controlError ma f =
    mapExceptT (\ma' -> ma' >>= either
                                  (\e -> maybe
                                           (runExceptT (f e))
                                           (pure . Left . review ioException)
                                           (fromException (toException e) :: Maybe IOException))
                                  (pure . Right)) ma
  accept :: ExceptT e m a -> ExceptT SomeNonPseudoException m a
  accept = withExceptT convert
    where convert :: e -> SomeNonPseudoException
          convert e =
            case preview ioException e of
              Just ioe -> fromJust (fromException (toException ioe))
              Nothing ->
                case preview nonIOException e of
                  Just e' -> e'
                  -- If we can't use IOException or the NonIOException
                  -- use the Exception instance.  I'm not fully certain
                  -- what the implications of this happening are.
                  Nothing -> fromJust (fromException (toException e))

#else
instance Monad m => ErrorControl SomeNonPseudoException (ExceptT SomeNonPseudoException m) (ExceptT IOException m) where
  controlError :: ExceptT SomeNonPseudoException m a -> (SomeNonPseudoException -> ExceptT IOException m a) -> ExceptT IOException m a
  controlError ma f =
    -- My process...
    -- (undefined :: ExceptT IOException m a)
    -- mapExceptT (undefined :: m (Either SomeNonPseudoException a) -> m (Either IOException a)) ma
    -- mapExceptT (\ma' -> ma' >>= (undefined :: (Either SomeNonPseudoException a) -> m (Either IOException a))) ma
    -- mapExceptT (\ma' -> ma' >>= either (undefined :: SomeNonPseudoException -> m (Either IOException a)) (undefined :: a -> m (Either IOException a))) ma
    -- mapExceptT (\ma' -> ma' >>= either ((undefined :: SomeException -> m (Either IOException a)) . toException) (pure . Right)) ma
    -- mapExceptT (\ma' -> ma' >>= either (\e -> (undefined :: Maybe IOException -> m (Either IOException a)) $ fromException $ toException e) (pure . Right)) ma
    -- mapExceptT (\ma' -> ma' >>= either (\e -> (maybe (undefined :: m (Either IOException a)) (undefined :: IOException -> m (Either IOException a))) $ fromException $ toException e) (pure . Right)) ma
    -- mapExceptT (\ma' -> ma' >>= either (\(e :: SomeNonPseudoException) -> (maybe ((undefined :: ExceptT IOException m a -> m (Either IOException a)) (f e)) (pure . Left)) $ fromException $ toException e) (pure . Right)) ma
    mapExceptT (\ma' -> ma' >>= either (\e -> maybe (runExceptT (f e)) (pure . Left) $ fromException $ toException e) (pure . Right)) ma
  accept :: ExceptT IOException m a -> ExceptT SomeNonPseudoException m a
  -- This will work because IOException is a non-pseudo exception
  accept = withExceptT (fromJust . fromException . toException)
#endif
