-- | Generate debug messages - verbosity controls, indentation.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -Wredundant-constraints #-}

module Extra.Debug2
    ( HasTraceInfo(verbosityLens, prefixLens)
    , message
    , trace
    , indented
    , verbosity
    ) where

import Control.Lens ((%=), (.=), Lens', use)
import Control.Monad (when)
import Control.Monad.RWS
import Data.List (intercalate)
import qualified Debug.Trace
import Language.Haskell.TH.Instances ()
import System.Log.Logger (Priority(..))

-- Perform an action with modified state.
bracketState :: MonadState s m => Lens' s a -> (a -> a) -> m r -> m r
bracketState lns f action = do
  a0 <- use lns
  lns %= f
  r <- action
  lns .= a0
  return r

class HasTraceInfo a where
    verbosityLens :: Lens' a Priority
    prefixLens :: Lens' a String

-- | If the verbosity argument monad's verbosity level exceeds the verbosity argument,
-- prepend the current indentation string to the lines of a message
-- and output it.
message :: (HasTraceInfo s, MonadState s m) => (String -> m ()) -> Priority -> String -> m ()
message f p s = do
  v <- use verbosityLens
  when (p >= v) $ do
    i <- use prefixLens
    f (intercalate "\n" $ fmap (i ++) (lines s))

trace :: (HasTraceInfo s, MonadState s m) => String -> m ()
trace = message (\s -> Debug.Trace.trace s (return ())) DEBUG

-- | Perform an action with added indentation
indented :: (HasTraceInfo s, MonadState s m) => String -> m r -> m r
indented s action = bracketState prefixLens (<> s) action

-- | Perform an action with modified verbosity
verbosity :: (HasTraceInfo s, MonadState s m) => Priority -> m r -> m r
verbosity v action = bracketState verbosityLens (const v) action
