-- | Declarations pulled out of th-unify

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Extra.Debug
    ( HasMessageInfo(..)
    , Verbosity(message)
    , quietly, noisily, indented
    , R(..))
    where

import Control.Lens (Lens', makeLenses, over, view)
import Control.Monad (when)
import Control.Monad.Reader (local, MonadReader, ReaderT)
import Control.Monad.RWS (RWST)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Char (isSpace)
import Data.List (dropWhileEnd, intercalate)
import Instances.TH.Lift ()
import Language.Haskell.TH.Instances ()
import System.IO (hPutStrLn, stderr)

class HasMessageInfo a where
    verbosity :: Lens' a Int
    prefix :: Lens' a String

-- | Class of monads with a verbosity level and a stored indentation string.
class (HasMessageInfo r, MonadReader r m) => Verbosity r m where
  message :: Int -> String -> m ()
  -- ^ If the monad's verbosity level exceeds the verbosity argument,
  -- prepend the current indentation string to the lines of a message
  -- and output it.

instance (MonadIO m, HasMessageInfo r, Monoid w) => Verbosity r (RWST r w s m) where
  message minv s = do
    v <- view verbosity
    p <- view prefix
    when (v >= minv) $ (liftIO . hPutStrLn stderr . indent p) s

instance (MonadIO m, HasMessageInfo r) => Verbosity r (ReaderT r m) where
  message minv s = do
    v <- view verbosity
    p <- view prefix
    liftIO $ putStrLn ("v=" ++ show v ++ " vmin=" ++ show minv)
    when (v >= minv) $ (liftIO . putStrLn . indent p) s

-- | Indent the lines of a message with a prefix
indent :: String -> String -> String
indent pre s = intercalate "\n" $ fmap (dropWhileEnd isSpace . (pre ++)) (lines s)

-- | If the current verbosity level is >= minv perform the action with
-- additional indentation.
indented :: (HasMessageInfo r, MonadReader r m) => Int -> m a -> m a
indented minv action = do
  (v :: Int) <- view verbosity
  if v >= minv then local (over prefix ("  " ++)) action else action

-- | Perform the action with reduced verbosity
quietly :: (HasMessageInfo r, MonadReader r m) => Int -> m a -> m a
quietly n = local (over verbosity (\i -> i - n))

-- | Perform the action with increased verbosity
noisily :: (HasMessageInfo r, MonadReader r m) => Int -> m a -> m a
noisily n = local (over verbosity (+ n))

-- | A type with a HasMessageInfo instance to use in the Reader or RWS monad.
data R
    = R
      { _verbosityR :: Int
      , _prefixR :: String
      }

$(makeLenses ''R)

instance HasMessageInfo R where
    verbosity = verbosityR
    prefix = prefixR
