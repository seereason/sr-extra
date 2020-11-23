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
import Data.List (intercalate)
import Data.Maybe (fromMaybe, mapMaybe)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup((<>)))
#endif
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
alog priority msg = liftIO $ do
  time <- getCurrentTime
  logM (modul callStack) priority $
    logString time priority msg

logString  :: HasCallStack => UTCTime -> Priority -> String -> String
logString time priority msg =
#if defined(darwin_HOST_OS)
  take 2002 $
#else
  take 60000 $
#endif
    msg

-- | Format the location of the nth level up in a call stack
modul :: CallStack -> String
modul stack =
  case dropWhile (\(_, SrcLoc {..}) -> srcLocModule == "Extra.Log") (getCallStack stack) of
    [] -> "???"
    [(_alog, SrcLoc {..})] -> srcLocModule <> ":" <> show srcLocStartLine
    ((_, SrcLoc {..}) : (fn, _) : _) -> srcLocModule <> "." <> fn <> ":" <> show srcLocStartLine

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
