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
    , logIOError

      -- * UnexceptionalIO extensions
    , HasSomeNonPseudoException(fromSomeNonPseudoException)
    , lyftIO

    , module Control.Monad.Except
    ) where

import Control.Exception ({-evaluate,-} Exception, IOException, SomeException(..))
import Control.Monad.Except
import Control.Monad.RWS (RWST)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans (MonadTrans(lift), liftIO)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Writer (WriterT)
import Data.Typeable (typeOf)
import Extra.Log (logException, Priority(ERROR))
import UnexceptionalIO (fromIO, SomeNonPseudoException, Unexceptional)
import qualified UnexceptionalIO as UIO (lift)

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

logIOError :: (MonadIO m, MonadError e m) => m a -> m a
logIOError = handleError (\e -> liftIO ($logException ERROR (pure e)) >> throwError e)

class HasSomeNonPseudoException e where
  fromSomeNonPseudoException :: SomeNonPseudoException -> e
instance HasSomeNonPseudoException SomeNonPseudoException where
  fromSomeNonPseudoException = id

lyftIO :: (MonadError e m, Unexceptional m, HasSomeNonPseudoException e) => IO a -> m a
--lyftIO io = runExceptT (lyftIO'' io) >>= either throwError return
--lyftIO io = runExceptT (withExceptT fromSomeNonPseudoException (lyftIO' io)) >>= either throwError return
--lyftIO io = runExceptT (withExceptT fromSomeNonPseudoException (fromIO io >>= liftEither)) >>= either throwError return
lyftIO io = fromIO io >>= either (throwError . fromSomeNonPseudoException) return

--lyftIO'' :: (Unexceptional m, HasSomeNonPseudoException e) => IO a -> ExceptT e m a
--lyftIO'' io = withExceptT fromSomeNonPseudoException (lyftIO' io)

--lyftIO' :: Unexceptional m => IO a -> ExceptT SomeNonPseudoException m a
--lyftIO' io = fromIO io >>= liftEither

instance Unexceptional m => Unexceptional (ExceptT e m) where
  lift = lift . UIO.lift
instance Unexceptional m => Unexceptional (ReaderT r m) where
  lift = lift . UIO.lift
instance (Unexceptional m, Monoid w) => Unexceptional (RWST r w s m) where
  lift = lift . UIO.lift
instance Unexceptional m => Unexceptional (StateT s m) where
  lift = lift . UIO.lift
instance (Unexceptional m, Monoid w) => Unexceptional (WriterT w m) where
  lift = lift . UIO.lift
