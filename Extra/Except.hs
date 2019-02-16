{-# LANGUAGE CPP, ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts, OverloadedStrings, RankNTypes, ScopedTypeVariables, StandaloneDeriving #-}
{-# OPTIONS -Wall -Wredundant-constraints -Wno-orphans #-}

module Extra.Except
    ( -- * Control.Exception extensions
      withException
    , displaySomeExceptionType
      -- * Control.Monad.Except extensions
    , tryError
    , withError -- , mapError'
    , HasIOException(fromIOException)
    , MonadIOError
    , liftIOError
    , tryIOError
    , HasLoc(withLoc)
    -- * Re-exports
    , module Control.Monad.Except
    ) where

import Control.Exception ({-evaluate,-} Exception, IOException, SomeException(..), try)
import Control.Monad.Except
import Data.Data (typeOf)
import Language.Haskell.TH.Syntax (Loc)

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

withError :: (MonadError e m, MonadError e' m) => (e -> e') -> m a -> m a
withError f action = tryError action >>= either (throwError . f) return

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

-- | Modify an exception to include a source code location:
-- e.g. @withError (withLoc $here) $ tryError action@.
class HasLoc e where withLoc :: Loc -> e -> e
