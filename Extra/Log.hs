{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP, TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module Extra.Log
  ( alog
  , Priority(..)
#if !__GHCJS__
  , logException
  , logQ
#endif
  ) where

import Control.Lens(ix, preview, to)
import Control.Monad.Except (MonadError(catchError, throwError))
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Time (getCurrentTime, UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import GHC.Stack (CallStack, callStack, getCallStack, HasCallStack, SrcLoc(..))
#if !__GHCJS__
import Language.Haskell.TH (ExpQ, Exp, Loc(..), location, pprint, Q)
import qualified Language.Haskell.TH.Lift as TH (Lift(lift))
import Language.Haskell.TH.Instances ()
#endif
import System.Log.Logger (Priority(..), logM, rootLoggerName)

alog :: (MonadIO m, HasCallStack) => Priority -> String -> m ()
alog priority msg = alog' 2 priority msg

alog' :: (MonadIO m, HasCallStack) => Int -> Priority -> String -> m ()
alog' pop priority msg = liftIO $ do
  time <- liftIO getCurrentTime
  liftIO $
    logM rootLoggerName priority $
      logString pop time priority msg

logString  :: HasCallStack => Int -> UTCTime -> Priority -> String -> String
logString pop time priority msg =
    unwords $ [timestring, fromMaybe "???" (modul callStack pop), "-", take 60000 msg] <> bool ["(" <> show priority <> ")"] [] (priority == DEBUG)
    where timestring = formatTime defaultTimeLocale "%T%4Q" time

-- | Format the location of the nth level up in a call stack
modul :: CallStack -> Int -> Maybe String
modul stack pop =
  preview (to getCallStack . ix pop . to prettyLoc) stack
  where
    prettyLoc (_s, SrcLoc {..}) =
      foldr (++) ""
        [ srcLocModule, ":"
        , show srcLocStartLine {-, ":"
        , show srcLocStartCol-} ]

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
--formatTimeCombined :: FormatTime t => t -> String
--formatTimeCombined = formatTime defaultTimeLocale "%d/%b/%Y:%H:%M:%S %z"

#if !__GHCJS__
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

logQ :: ExpQ
logQ = do
  loc <- location
  [|\priority message ->
       alog $(TH.lift (show (loc_module loc))) priority
         ($(TH.lift (show (loc_module loc) <> ":" <> show (fst (loc_start loc)))) <> " - " <> message)|]
#endif
