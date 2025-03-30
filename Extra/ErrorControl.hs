-- | From the PureScript Error.Control package by Luka Jacobowitz.

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Extra.ErrorControl {-# DEPRECATED "Use package sr-errors" #-}
  ( MonadError(throwError) -- re-export
  , ErrorControl(controlError, accept)
  , intercept
  , trial
  , trialT
  , absolve
  , assure
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Except
  (catchError, ExceptT(ExceptT), MonadError, runExceptT, throwError, withExceptT)
import Control.Monad.Identity (Identity(Identity), runIdentity)
import Control.Monad.Reader (mapReaderT, ReaderT(ReaderT), runReaderT)
import Control.Monad.RWS (mapRWST, runRWST, RWST(RWST))
import Control.Monad.State (mapStateT, runStateT, StateT(StateT))
import Control.Monad.Writer (mapWriterT, runWriterT, WriterT(WriterT))
import Control.Exception (IOException)
import UnexceptionalIO.Trans (run, UIO, unsafeFromIO)

class (MonadError e m, Monad n) => ErrorControl e m n where
  controlError :: m a -> (e -> n a) -> n a
  accept :: n a -> m a

instance ErrorControl e (Either e) Identity where
  controlError ma f = either f Identity ma
  accept = Right . runIdentity

instance ErrorControl IOException IO UIO where
  controlError ma f = unsafeFromIO (ma `catchError` (accept . f))
  accept = run

instance Monad m => ErrorControl e (ExceptT e m) m where
  controlError ma f = runExceptT ma >>= either f pure
  accept = lift

-- | Resolve the error on the left side of an Either.
instance Monad m => ErrorControl (Either e1 e2) (ExceptT (Either e1 e2) m) (ExceptT e2 m) where
  controlError :: ExceptT (Either e1 e2) m a -> (Either e1 e2 -> ExceptT e2 m a) -> ExceptT e2 m a
  controlError ma f =
    ExceptT (pivot <$> runExceptT ma) >>= either pure (f . Left)
    where
      pivot :: Either (Either a b) c -> Either b (Either c a)
      pivot = either (either (Right . Right) Left) (Right . Left)
  accept :: ExceptT e2 m a -> ExceptT (Either e1 e2) m a
  accept = withExceptT Right

#if 0
-- | Resolve the error on the right side of an Either.  (It turns out
-- this actually does conflict with the one above.)
instance Monad m => ErrorControl (Either e1 e2) (ExceptT (Either e1 e2) m) (ExceptT e1 m) where
  controlError ma f =
    ExceptT (pivot <$> runExceptT ma) >>= either (f . Right) pure
    where
      pivot :: Either (Either a b) c -> Either a (Either b c)
      pivot = either (either Left (Right . Left)) (Right . Right)
  accept = withExceptT Left
#endif

instance ErrorControl e m n => ErrorControl e (StateT s m) (StateT s n) where
  controlError sma f = StateT (\s -> controlError (runStateT sma s) (\e -> runStateT (f e) s))
  accept = mapStateT accept

instance ErrorControl e m n => ErrorControl e (ReaderT r m) (ReaderT r n) where
  controlError rma f = ReaderT (\r -> controlError (runReaderT rma r) (\e -> runReaderT (f e) r))
  accept = mapReaderT accept

instance (ErrorControl e m n, Monoid w) => ErrorControl e (WriterT w m) (WriterT w n) where
  controlError wma f = WriterT (controlError (runWriterT wma) (runWriterT . f))
  accept = mapWriterT accept

instance (ErrorControl e m n, Monoid w) => ErrorControl e (RWST r w s m) (RWST r w s n) where
  controlError rwsma f = RWST (\r s -> controlError (runRWST rwsma r s) (\e -> runRWST (f e) r s))
  accept = mapRWST accept

-- | Enhanced 'handleError'
intercept :: ErrorControl e m n => m a -> (e -> a) -> n a
intercept fa f = controlError fa (pure . f)

-- | Enhanced 'try'
trial :: ErrorControl e m n => m a -> n (Either e a)  -- try
trial fa = intercept (fmap Right fa) Left

trialT :: ErrorControl e m n => m a -> ExceptT e n a
trialT fa = ExceptT (trial fa)

absolve :: ErrorControl e m n => n (Either e a) -> m a
absolve gea = accept gea >>= (either throwError pure)

assure :: ErrorControl e m n => n a -> (a -> e) -> (a -> Bool) -> m a
assure ga err predicate =
  accept ga >>= (\a -> if predicate a then pure a else throwError (err a))
