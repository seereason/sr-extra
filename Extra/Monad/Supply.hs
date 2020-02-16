{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Support for computations which consume values from a (possibly infinite)
-- supply. See <http://www.haskell.org/haskellwiki/New_monads/MonadSupply> for
-- details.
module Extra.Monad.Supply
( MonadSupply (..)
, SupplyT
, Supply
, evalSupplyT
, evalSupply
, runSupplyT
, runSupply
, mapSupplyT
, supplies
) where

import Control.Exception (throw)
import Control.Monad.Catch
import Control.Monad.Fix
import Control.Monad.Identity
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Morph (MFunctor(..))
import qualified Data.Semigroup as Sem
import Extra.ErrorControl

class Monad m => MonadSupply s m | m -> s where
  supply :: m s
  peek :: m s
  exhausted :: m Bool

-- | Supply monad transformer.
newtype SupplyT s m a = SupplyT (StateT [s] m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadFix)

instance MonadError e m => MonadError e (SupplyT s m) where
  throwError = SupplyT . lift . throwError
  catchError (SupplyT action) handler = SupplyT (action `catchError` (\e -> (\(SupplyT m) -> m) $ handler e))

-- | Supply monad.
newtype Supply s a = Supply (SupplyT s Identity a)
  deriving (Functor, Applicative, Monad, MonadSupply s)


instance Monad m => MonadSupply s (SupplyT s m) where
  supply = SupplyT $ do ~(x:xs) <- get
                        put xs
                        return x
  peek = SupplyT $ gets head
  exhausted = SupplyT $ gets null

-- Monad transformer instances
instance (MonadSupply s m) => MonadSupply s (ExceptT e m) where
  supply = lift supply
  peek = lift peek
  exhausted = lift exhausted

instance MonadSupply s m => MonadSupply s (StateT st m) where
  supply = lift supply
  peek = lift peek
  exhausted = lift exhausted

instance MonadSupply s m => MonadSupply s (ReaderT r m) where
  supply = lift supply
  peek = lift peek
  exhausted = lift exhausted

instance (Monoid w, MonadSupply s m) => MonadSupply s (WriterT w m) where
  supply = lift supply
  peek = lift peek
  exhausted = lift exhausted

instance (MonadSupply s m, Monoid w) => MonadSupply s (RWST r w st m) where
  supply = lift supply
  peek = lift peek
  exhausted = lift exhausted

instance MFunctor (SupplyT a) where
  hoist f (SupplyT s) = SupplyT (hoist f s)

-- | Monoid instance for the supply monad. Actually any monad/monoid pair
-- gives rise to this monoid instance, but we can't write its type like that
-- because it would conflict with existing instances provided by Data.Monoid.
--instance (Monoid a, Monad m) => Monoid (m a) where
instance Sem.Semigroup a => Sem.Semigroup (Supply s a) where
  m1 <> m2 = do
    x1 <- m1
    x2 <- m2
    return (x1 Sem.<> x2)

instance (Monoid a) => Monoid (Supply s a) where
  mempty = return mempty
  mappend = (<>)

instance ErrorControl e m n => ErrorControl e (SupplyT s m) (SupplyT s n) where
  controlError :: SupplyT s m a -> (e -> SupplyT s n a) -> SupplyT s n a
  controlError ma f = SupplyT (controlError (unSupplyT ma) (unSupplyT . f)) where unSupplyT (SupplyT s) = s
  accept (SupplyT n) = SupplyT (accept n)

instance (MonadCatch m, MonadIO m) => MonadCatch (SupplyT i m) where
  catch (SupplyT io) f = SupplyT (catch io (unSupplyT . f))
    where unSupplyT (SupplyT s) = s

instance (MonadThrow m, MonadIO m) => MonadThrow (SupplyT i m) where
  throwM = liftIO . throw

-- | Get n supplies.
supplies :: MonadSupply s m => Int -> m [s]
supplies n = replicateM n supply

evalSupplyT :: Monad m => SupplyT s m a -> [s] -> m a
evalSupplyT (SupplyT s) = evalStateT s

evalSupply :: Supply s a -> [s] -> a
evalSupply (Supply s) = runIdentity . evalSupplyT s

runSupplyT :: SupplyT s m a -> [s] -> m (a,[s])
runSupplyT (SupplyT s) = runStateT s

runSupply :: Supply s a -> [s] -> (a,[s])
runSupply (Supply s) = runIdentity . runSupplyT s

mapSupplyT :: (m (a, [s]) -> n (b, [s])) -> SupplyT s m a -> SupplyT s n b
mapSupplyT f (SupplyT s) = SupplyT (mapStateT f s)
