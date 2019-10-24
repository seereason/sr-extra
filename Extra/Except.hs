{-# LANGUAGE CPP, DeriveAnyClass, FunctionalDependencies, OverloadedStrings, TemplateHaskell, UndecidableInstances #-}
{-# OPTIONS -Wall -Wredundant-constraints -Wno-orphans #-}

module Extra.Except
    ( -- * Control.Exception extensions
      withException
    , displaySomeExceptionType
      -- * Control.Monad.Except extensions
    , tryError
    , withError
    , mapError
    , handleError
    , HasIOException(fromIOException)
    , IOException'(..)
#if 1
    , Tried(unTried)
    , tryIO
    , logIOError
#else
    , LyftIO(lyftIO, IOMonad, ErrorType)
    , tryLiftIO
    , logLiftIO
    , lyftIO'
    , LyftIO'
    , LyftIO2
    , lyftIO2

    , MonadIOError
    , liftIOError
    , tryIOError
    , logIOError
#endif
    , module Control.Monad.Except
    ) where

import Control.Exception ({-evaluate,-} Exception, IOException, SomeException(..))
import Control.Monad.Catch
import Control.Monad.Except
--import Control.Monad.Reader (ReaderT)
--import Control.Monad.RWS (RWST)
--import Control.Monad.State (StateT)
--import Control.Monad.Writer (WriterT)
import Data.Typeable (typeOf)
import Extra.Log (logException, Priority(ERROR))

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

-- | Modify the value (but not the type) of an error
withError :: MonadError e m => (e -> e) -> m a -> m a
withError f action = tryError action >>= either (throwError . f) return

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
class HasIOException e where fromIOException :: IOException -> e

newtype IOException' = IOException' IOException
instance Show IOException' where show (IOException' e) = "(IOException' " <> show (show e) <> ")"
instance HasIOException IOException' where fromIOException = IOException'

#if 1

-- Newtype wrapper around values that are the result of an IO
-- operation invoked with try.  The Tried constructor is private
-- so it can only appear as the result of the lyftIO operation.
newtype Tried a = Tried {unTried :: a} deriving Functor

tryIO :: (MonadIO m, MonadCatch m, Exception e, MonadError e m) => m a -> ExceptT e m (Tried a)
tryIO io = lift (try io >>= liftEither . fmap Tried)

logIOError :: (MonadIO m, MonadError e m) => m a -> m a
logIOError = handleError (\e -> liftIO ($logException ERROR (pure e)) >> throwError e)

#else

class (MonadIO (IOMonad m),
       -- The idea of LyftIO is to be a wrapper around a MonadIO
       -- instance that is not itself a MonadIO instance. that means
       -- you have to use lyftIO to run IO rather than liftIO, and
       -- lyftIO always catches exceptions.
       Exception (ErrorType m),
       MonadCatch m,
       MonadCatch (IOMonad m),
       MonadError (ErrorType m) m,
       MonadError (ErrorType m) (IOMonad m),
       -- IOMonad m must have the same error type as m, this is
       -- generally done through error constraints like
       -- HasIOException.
       HasIOException (ErrorType m)) => LyftIO m where
  type ErrorType m
  type IOMonad m :: * -> *
  lyftIO :: IOMonad m a -> m a

instance {-# Overlapping #-} (MonadIO m,
                              e ~ ErrorType m,
                              Exception e,
                              MonadCatch m,
                              MonadError e (IOMonad (ExceptT e m)),
                              HasIOException e) => LyftIO (ExceptT e m) where
  type ErrorType (ExceptT e m) = e
  type IOMonad (ExceptT e m) = m
  lyftIO io = lift (try io >>= liftEither)

instance LyftIO m => LyftIO (ReaderT r m) where
  type ErrorType (ReaderT r m) = ErrorType m
  type IOMonad (ReaderT r m) = IOMonad m
  lyftIO = lift . lyftIO
instance LyftIO m => LyftIO (StateT s m) where
  type ErrorType (StateT s m) = ErrorType m
  type IOMonad (StateT s m) = IOMonad m
  lyftIO = lift . lyftIO
instance (Monoid w, LyftIO m) => LyftIO (WriterT w m) where
  type ErrorType (WriterT w m) = ErrorType m
  type IOMonad (WriterT w m) = IOMonad m
  lyftIO = lift . lyftIO
instance (Monoid w, LyftIO m) => LyftIO (RWST r w s m) where
  type ErrorType (RWST r w s m) = ErrorType m
  type IOMonad (RWST r w s m) = IOMonad m
  lyftIO = lift . lyftIO

-- | LyftIO analog to the 'try' function.
tryLiftIO :: LyftIO m => IOMonad m a -> m (Either (ErrorType m) a)
tryLiftIO = tryError . lyftIO

logLiftIO :: forall m a. LyftIO m => m a -> m a
logLiftIO = handleError (\e -> lyftIO ($logException ERROR (pure e) :: IOMonad m (ErrorType m)) >> throwError e)

type LyftIO' m = (LyftIO m, IOMonad m ~ ExceptT (ErrorType m) IO)

-- | Lift an IO action into any LyftIO instance.  Well, almost any
-- LyftIO instance.
lyftIO' :: forall m a. (LyftIO' m) => IO a -> m a
lyftIO' io = lyftIO (withExceptT f (liftIO io))
  where f :: IOException' -> ErrorType m
        f = (fromIOException . (\(IOException' e) -> e))

type LyftIO2 e m = (LyftIO' m, e ~ ErrorType m)

-- | Lift an @IOMonad m a@ action into @m a@.
lyftIO2 :: LyftIO2 e m => IOMonad m a -> m a
lyftIO2 action = lyftIO action

-- Backwards compatibiity

type MonadIOError e m = (LyftIO' m, e ~ ErrorType m)

liftIOError :: MonadIOError e m => IO a -> m a
liftIOError = lyftIO'

tryIOError :: MonadIOError e m => IO a -> m (Either e a)
tryIOError = tryError . liftIOError

logIOError :: MonadIOError e m => m a -> m a
logIOError = handleError (\e -> liftIOError ($logException ERROR (pure e)) >> throwError e)

#endif
