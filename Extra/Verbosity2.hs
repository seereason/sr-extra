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

module Extra.Verbosity2
    ( message, quietly, noisily, indented )
    where

import Control.Lens (_2, _3, (.=), (%=), at, Lens', lens, makeLenses, non, over, Traversal', use, view)
import Control.Monad (MonadPlus, msum, when)
import Control.Monad.Reader (local, MonadReader, ReaderT)
import Control.Monad.RWS (evalRWST, listens, {-local,-} RWST, tell)
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Char (isSpace)
import Data.Data (Data)
import Data.Generics ({-Data,-} everywhere, listify, mkT, Typeable)
import Data.List (dropWhileEnd, intercalate)
import Data.Map as Map (insert, lookup, Map, null, toList)
import Data.Set as Set (difference, fromList, Set, singleton, toList, union, unions)
import Instances.TH.Lift ()
import Language.Haskell.TH
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.PprLib (Doc, hcat, hsep, ptext, to_HPJ_Doc)
import Language.Haskell.TH.Syntax
import qualified Text.PrettyPrint as HPJ
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
