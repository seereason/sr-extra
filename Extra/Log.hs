{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP, RankNTypes, TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module Extra.Log {-# DEPRECATED "Use SeeReason.Log in sr-log" #-}
  ( -- * Logging
    alog
  , alogs
  , printLoc
  , putLoc
  , loc
  , loc'
  , logString
    -- * Elapsed time
  , HasSavedTime(..)
  , alog'
  , logString'
    -- * Re-exports
  , Priority(..)
  ) where

import Control.Lens((.=), ix, Lens', preview, to, use)
import Control.Monad (when)
import Control.Monad.State (MonadState)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup((<>)))
#endif
import Data.Time (diffUTCTime, getCurrentTime, UTCTime)
#if MIN_VERSION_time(1,9,0)
import Data.Time.Format (formatTime, defaultTimeLocale)
#endif
import GHC.Stack (CallStack, callStack, getCallStack, HasCallStack, SrcLoc(..))
#if !__GHCJS__
import Language.Haskell.TH.Instances ()
#endif
import System.Log.Logger (getLevel, getLogger, getRootLogger, logL, Priority(..))
import Text.Printf (printf)

alog :: (MonadIO m, HasCallStack) => Priority -> String -> m ()
alog priority msg = liftIO $ do
  -- time <- getCurrentTime
  logger <- getRootLogger
  logL logger priority (logString msg)

alogs :: forall m. (MonadIO m, HasCallStack) => Priority -> [String] -> m ()
alogs priority msgs = alog priority (unwords msgs)

logString  :: HasCallStack => String -> String
logString msg =
#if defined(darwin_HOST_OS)
  take 2002 $
#else
  take 60000 $
#endif
  loc <> " - " <> msg

printLoc :: (Show a, HasCallStack, MonadIO m) => a -> m ()
printLoc x = putLoc >> liftIO (print x)

putLoc :: (HasCallStack, MonadIO m) => m ()
putLoc = liftIO (putStr (loc <> " - "))

-- | Format the location of the nth level up in a call stack
loc :: HasCallStack => String
loc =
  case dropWhile (\(_, SrcLoc {..}) -> srcLocModule == "Extra.Log") (getCallStack callStack) of
    [] -> "(no CallStack)"
    [(_alog, SrcLoc {..})] -> srcLocModule <> ":" <> show srcLocStartLine
    ((_, SrcLoc {..}) : (fn, _) : _) -> srcLocModule <> "." <> fn <> ":" <> show srcLocStartLine

-- | Format the location of the nth level up in a call stack
loc' :: CallStack -> Int -> Maybe String
loc' stack n =
  preview (to getCallStack . ix n . to prettyLoc) stack
  where
    prettyLoc (_s, SrcLoc {..}) =
      foldr (++) ""
        [ srcLocModule, ":"
        , show srcLocStartLine {-, ":"
        , show srcLocStartCol-} ]

class HasSavedTime s where savedTime :: Lens' s UTCTime
instance HasSavedTime UTCTime where savedTime = id

alog' :: forall s m. (MonadIO m, HasSavedTime s, HasCallStack, MonadState s m) => Priority -> String -> m ()
alog' priority msg = do
  level <- getLevel <$> liftIO (maybe getRootLogger getLogger (loc' callStack 1))
  prev <- use savedTime
  time <- liftIO getCurrentTime
  logger <- liftIO getRootLogger
  when (level <= Just priority) (savedTime .= time)
  liftIO $
    logL logger priority $
      logString' prev time priority msg

logString'  :: UTCTime -> UTCTime -> Priority -> String -> String
logString' prev time priority msg =
#if defined(darwin_HOST_OS)
  take 2002 $
#else
  take 60000 $
#endif
    unwords $ [timestring, fromMaybe "???" (loc' callStack 1), "-", msg] <> bool [] ["(" <> show priority <> ")"] (priority == DEBUG)
    where timestring =
#if MIN_VERSION_time(1,9,0)
            formatTime defaultTimeLocale "%T%4Q"
#else
            (("elapsed: " <>) . (printf "%.04f" :: Double -> String) . fromRational . toRational)
#endif
              (diffUTCTime time prev)
