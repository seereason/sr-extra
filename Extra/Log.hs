{-# OPTIONS -Wall #-}

module Extra.Log
  ( alog
  , Priority(..)
  , logException
  ) where

import Control.Monad.Except (MonadError(catchError, throwError))
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Time (getCurrentTime)
import Data.Time.Format (FormatTime(..), formatTime, defaultTimeLocale)
import Language.Haskell.TH (ExpQ, Exp, Loc(..), location, pprint, Q)
import Language.Haskell.TH.Instances ()
import qualified Language.Haskell.TH.Lift as TH (Lift(lift))
import System.Log.Logger (Priority(..), logM)

alog :: MonadIO m => String -> Priority -> String -> m ()
alog modul priority msg = liftIO $ do
  time <- getCurrentTime
  logM modul priority $ unwords [formatTimeCombined time, msg]

-- | Format the time as describe in the Apache combined log format.
--   http://httpd.apache.org/docs/2.2/logs.html#combined
--
-- The format is:
--   [day/month/year:hour:minute:second zone]
--    day = 2*digit
--    month = 3*letter
--    year = 4*digit
--    hour = 2*digit
--    minute = 2*digit
--    second = 2*digit
--    zone = (`+' | `-') 4*digit
--
-- (Copied from happstack-server)
formatTimeCombined :: FormatTime t => t -> String
formatTimeCombined = formatTime defaultTimeLocale "%d/%b/%Y:%H:%M:%S %z"

-- | Create an expression of type (MonadIO m => Priority -> m a -> m
-- a) that we can apply to an expression so that it catches, logs, and
-- rethrows any exception.
logException :: ExpQ
logException =
    [| \priority action ->
         action `catchError` (\e -> do
                                liftIO (logM (loc_module $__LOC__)
                                             priority
                                             ("Logging exception: " <> (pprint $__LOC__) <> " -> " ++ show e))
                                throwError e) |]

__LOC__ :: Q Exp
__LOC__ = TH.lift =<< location
