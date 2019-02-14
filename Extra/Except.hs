module Extra.Except
    ( tryError
    -- * Re-exports
    , module Control.Monad.Except
    ) where

import Control.Exception (evaluate)
import Control.Monad.Except

-- | MonadError analog to the 'try' function.
tryError :: MonadError e m => m a -> m (Either e a)
tryError action = (Right <$> action) `catchError` (pure . Left)

-- | Lift an IO operation and catch any IOException
tryIOError :: (MonadIO m, MonadError e m) => IO a -> m (Either e a)
tryIOError action = tryError $ liftIO action
