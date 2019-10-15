{-# LANGUAGE CPP, DeriveAnyClass, OverloadedStrings, TemplateHaskell, UndecidableInstances #-}
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
    , MonadIOError
    , liftIOError
    , tryIOError
    , logIOError
    , HasLoc(withLoc)

    , module Control.Monad.Except
    ) where

import Control.Exception ({-evaluate,-} Exception, IOException, SomeException(..), try)
import Control.Monad.Except
import Control.Monad.Reader (ReaderT)
import Control.Monad.RWS (RWST)
import Control.Monad.State (StateT)
import Control.Monad.Writer (WriterT)
import Data.Data (typeOf)
import Extra.Log (logException)
import Language.Haskell.TH.Syntax (Loc)
import System.Log.Logger (Priority(ERROR))

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

-- | An alternative to MonadIO that catches all IOExceptions.
class (HasIOException e, MonadError e m) => MonadIOError e m where
  liftIOError :: IO a -> m a

-- | MonadIOError analog to the 'try' function.
tryIOError :: MonadIOError e m => IO a -> m (Either e a)
tryIOError = tryError . liftIOError

instance {-# Overlapping #-} (HasIOException e, MonadIO m) => MonadIOError e (ExceptT e m) where
  liftIOError io = liftIO (try io) >>= either (throwError . fromIOException) return

-- This instance overlaps with the ExceptT instance above, which is
-- preferred, hence the Overlappable.
#if 0
instance {-# Overlappable #-} (MonadIOError e m, MonadError e (t m), MonadTrans t) => MonadIOError e (t m) where
  liftIOError = lift . liftIOError
#else
instance MonadIOError e m => MonadIOError e (ReaderT r m) where liftIOError = lift . liftIOError
instance MonadIOError e m => MonadIOError e (StateT s m) where liftIOError = lift . liftIOError
instance (MonadIOError e m, Monoid w) => MonadIOError e (WriterT w m) where liftIOError = lift . liftIOError
instance (MonadIOError e m, Monoid w) => MonadIOError e (RWST r w s m) where liftIOError = lift . liftIOError
#endif

logIOError :: MonadIOError e m => m a -> m a
logIOError = handleError (\e -> liftIOError ($logException ERROR (pure e)) >> throwError e)

-- | Modify an exception to include a source code location:
-- e.g. @withError (withLoc $here) $ tryError action@.
class HasLoc e where withLoc :: Loc -> e -> e

#if 0
readFile' :: MonadIOError e m => FilePath -> m String
readFile' = liftIOError . readFile

example :: IO ()
example = do
  r <- runExceptT (readFile' "/etc/nonexistant" :: ExceptT IOException' IO String)
  putStrLn (show r <> " :: " <> show (typeOf r))
#endif
