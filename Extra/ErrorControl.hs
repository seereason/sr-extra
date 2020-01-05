-- | From the PureScript Error.Control package by Luka Jacobowitz.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Extra.ErrorControl
  ( MonadError(throwError) -- re-export
  , ErrorControl(Handled, controlError, accept)
  , intercept
  , trial
  , trialT
  , absolve
  , assure
  ) where

import Control.Monad.Except
  (catchError, ExceptT(ExceptT), lift, MonadError, runExceptT, throwError)
import Control.Monad.Identity (Identity(Identity), runIdentity)
import Control.Monad.Reader (mapReaderT, ReaderT(ReaderT), runReaderT)
import Control.Monad.RWS (mapRWST, runRWST, RWST(RWST))
import Control.Monad.State (mapStateT, runStateT, StateT(StateT))
import Control.Monad.Writer (mapWriterT, runWriterT, WriterT(WriterT))
import Control.Exception (IOException)
import UnexceptionalIO.Trans (run, UIO, unsafeFromIO)

class (MonadError e m, Monad (Handled m)) => ErrorControl e m where
  type Handled m :: * -> *
  controlError :: m a -> (e -> Handled m a) -> Handled m a
  accept :: Handled m a -> m a

instance ErrorControl e (Either e) where
  type Handled (Either e) = Identity
  controlError ma f = either f Identity ma
  accept = Right . runIdentity

instance ErrorControl IOException IO where
  type Handled IO = UIO
  controlError ma f = unsafeFromIO (ma `catchError` (accept . f))
  accept = run

instance Monad m => ErrorControl e (ExceptT e m) where
  type Handled (ExceptT e m) = m
  controlError ma f = runExceptT ma >>= either f pure
  accept = lift

instance ErrorControl e m => ErrorControl e (StateT s m) where
  type Handled (StateT s m) = StateT s (Handled m)
  controlError sma f = StateT (\s -> controlError (runStateT sma s) (\e -> runStateT (f e) s))
  accept = mapStateT accept

instance ErrorControl e m => ErrorControl e (ReaderT r m) where
  type Handled (ReaderT r m) = ReaderT r (Handled m)
  controlError rma f = ReaderT (\r -> controlError (runReaderT rma r) (\e -> runReaderT (f e) r))
  accept = mapReaderT accept

instance (ErrorControl e m, Monoid w) => ErrorControl e (WriterT w m) where
  type Handled (WriterT w m) = WriterT w (Handled m)
  controlError wma f = WriterT (controlError (runWriterT wma) (runWriterT . f))
  accept = mapWriterT accept

instance (ErrorControl e m, Monoid w) => ErrorControl e (RWST r w s m) where
  type Handled (RWST r w s m) = RWST r w s (Handled m)
  controlError rwsma f = RWST (\r s -> controlError (runRWST rwsma r s) (\e -> runRWST (f e) r s))
  accept = mapRWST accept

-- | Enhanced 'handleError'
intercept :: ErrorControl e m => m a -> (e -> a) -> Handled m a
intercept fa f = controlError fa (pure . f)

-- | Enhanced 'try'
trial :: ErrorControl e m => m a -> Handled m (Either e a)  -- try
trial fa = intercept (fmap Right fa) Left

trialT :: ErrorControl e m => m a -> ExceptT e (Handled m) a
trialT fa = ExceptT (trial fa)

absolve :: ErrorControl e m => Handled m (Either e a) -> m a
absolve gea = accept gea >>= (either throwError pure)

assure :: ErrorControl e m => Handled m a -> (a -> e) -> (a -> Bool) -> m a
assure ga err predicate =
  accept ga >>= (\a -> if predicate a then pure a else throwError (err a))
