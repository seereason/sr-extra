{-# LANGUAGE CPP, ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes, ScopedTypeVariables, StandaloneDeriving #-}
{-# OPTIONS -Wall -Wredundant-constraints -Wno-orphans #-}

module Extra.Except
    ( -- * Control.Exception extensions
      withException
    , displaySomeExceptionType
    , -- * Control.Monad.Except extensions
      tryError
    , HasIOException(fromIOException)
    , MonadIOError
    , tryIOError
    , liftIOError
    -- * Re-exports
    , module Control.Monad.Except
    ) where

import Control.Exception ({-evaluate,-} Exception, IOException, SomeException(..), try)
import Control.Monad.Except
import Data.Data (typeOf)
-- | Apply a function to whatever @Exception@ type is inside a
-- @SomeException@.
withException :: forall r. (forall e. Exception e => e -> r) -> SomeException -> r
withException f (SomeException e) = f e

-- | Use 'withException' to obtain the exception's type name.
displaySomeExceptionType :: SomeException -> String
displaySomeExceptionType = withException (show . typeOf)

-- | MonadError analog to the 'try' function.
tryError :: MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)

-- | This class includes an instance for IOException itself, so we
-- don't know whether the exception has been caught.  Because there is
-- an instance @MonadError IOException IO@, do NOT declare
-- @@
--   instance HasIOException IOException where fromIOException = id
-- @@
-- or the HasIOException constraint won't guarantee that we are
-- catching these exceptions.  DO create instances like
-- @@
--   data ProcessError
--       = IOException Text
--       | CommandFailure ExitCode
--       deriving (Data, Eq, Ord, Show, Generic, Serialize)
--
--  instance HasIOException ProcessError where fromIOException = IOException . pack . show
-- @@
class HasIOException e where fromIOException :: IOException -> e

-- | Constraints required by 'liftIOError' or 'tryIOError'
type MonadIOError e m = (MonadIO m, HasIOException e, MonadError e m)

-- So , which means
-- that with constraints @MonadIO m, MonadError e m@ it might be that
-- @m ~ IO@.  We want a signature for LiftEIO that excludes this
-- possibility so we know we have caught the "real" 'Exception'.
liftIOError :: MonadIOError e m => IO a -> m a
liftIOError action = liftIO (try action) >>= either (throwError . fromIOException) return

-- | Lift an IO operation and catch any IOException
tryIOError :: MonadIOError e m => IO a -> m (Either e a)
tryIOError = tryError . liftIOError
