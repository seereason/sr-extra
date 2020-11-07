{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Extra.Errors
  ( IsMember
  , Nub
  , OneOf(Empty, Val, NoVal)
  , Set(set)
  , Get(get)
  , oneOf
  , DeleteList
  , DeleteOneOf
  , Delete(delete)
  , Member
  , throwMember
  , liftMember
  , catchMember
  , tryMember
  , mapMember
  , runNullExceptT
  , runNullExcept
  , liftUIO
  , runOneOf -- I think this is the best one
  , runOneOf'
  , runOneOf''
  , Errors
  , Errors'
  , test
  , IOException
  , module Control.Monad.Except
  , module Extra.Except
  , module UnexceptionalIO.Trans
  ) where

import Control.Exception (fromException, IOException, toException)
import Control.Lens (Prism', prism', review)
import Control.Monad.Except (Except, ExceptT, MonadError, runExcept, runExceptT, throwError, withExceptT)
--import Extra.Except (mapError)
import Data.Type.Bool
--import Data.Type.Equality
import Data.Word (Word8)
import Data.SafeCopy
import qualified Data.Serialize as S (Serialize(get, put), getWord8, Put, PutM, Get)
import Data.Typeable (Typeable, typeOf)
import Data.Proxy
import Extra.Except (NonIOException(..), tryError)
import GHC.Stack (HasCallStack)
import UnexceptionalIO.Trans (fromIO, SomeNonPseudoException, Unexceptional)

type family IsMember x ys where
  IsMember x '[] = 'False
  IsMember x (x ': ys) = 'True
  IsMember x (y ': ys) = IsMember x ys
  IsMember x ys = IsMember x ys

--type family Member x es where
--  Member' x (OneOf xs) = Member' x xs

type Member e es = (IsMember e es ~ 'True, Get e es, Set e es, Delete e es)

type family Nub xs where
  Nub '[] = '[]
  Nub (x ': ys) = If (IsMember x ys) ys (x ': Nub ys)

data OneOf (n :: [k]) where
  Empty :: OneOf s
  Val   :: e -> OneOf (e ': s)
  NoVal :: OneOf s -> OneOf (e ': s)
  deriving Typeable

instance Show (OneOf '[]) where
  show Empty = "{}"

instance (Show e, Show (OneOf s)) => Show (OneOf (e ': s)) where
  show (Val e) = show e
  show (NoVal o) = show o
  show Empty  = "{}"

instance S.Serialize (OneOf '[]) where
  get :: S.Get (OneOf s)
  get = return Empty
  put :: OneOf s -> S.PutM ()
  put Empty = return ()
  put _ = error "fix warning"

instance (S.Serialize e, S.Serialize (OneOf s)) => S.Serialize (OneOf (e ': s)) where
  put :: OneOf (e ': s) -> S.PutM ()
  put (NoVal o) = S.put (0 :: Word8) >> S.put o
  put (Val e) = S.put (1 :: Word8) >> S.put e
  put _ = error "impossible"
  get :: S.Get (OneOf (e ': s))
  get = S.getWord8 >>= \case
    0 -> NoVal <$> S.get
    1 -> Val <$> S.get
    _ -> error "impossible"

instance SafeCopy (OneOf '[]) where
  version = 1
  kind = base
  getCopy :: S.Serialize (OneOf s) => Contained (S.Get (OneOf s))
  getCopy = contain S.get
  putCopy :: S.Serialize (OneOf s) => OneOf s -> Contained S.Put
  putCopy = contain . S.put
  errorTypeName _ = "()"

instance (SafeCopy e, S.Serialize e, Typeable e,  S.Serialize (OneOf s), Typeable s) => SafeCopy (OneOf (e ': s)) where
  version = 1
  kind = base
  getCopy :: Contained (S.Get (OneOf (e ': s)))
  getCopy = contain S.get
  putCopy :: OneOf (e ': s) -> Contained S.Put
  putCopy = contain . S.put
  errorTypeName = show . typeOf

class Set e xs where
  set :: e -> OneOf xs

instance Set e (e ': xs) where
  set e = Val e

instance {-# OVERLAPS #-} (IsMember e xs ~ 'True, Set e xs) => Set e (f ': xs) where
  set e = NoVal (set e)

class Get e xs where
  get :: OneOf xs -> Maybe e

instance {-# OVERLAPS #-} Get e (e ': xs) where
  get (Val e) = Just e
  get (NoVal _) = Nothing
  get Empty = error "impossible"

instance (IsMember e xs ~ 'True, Get e xs) => Get e (f ': xs) where
  get (NoVal o) = get o
  get (Val _e) = Nothing
  get Empty = error "impossible"

oneOf :: (Get e es, Set e es) => Prism' (OneOf es) e
oneOf = prism' set get

type family DeleteList e xs where
  DeleteList x '[] = '[]
  DeleteList x (x ': ys) = ys
  DeleteList x (y ': ys) = (y ': (DeleteList x ys))

type family DeleteOneOf e xs where
  DeleteOneOf x (OneOf ys) = OneOf (DeleteList x ys)

class Delete e xs where
  delete :: Proxy e -> OneOf xs -> DeleteOneOf e (OneOf xs)

instance Delete e (e ': xs) where
  delete _ (Val _e) = Empty
  delete _ (NoVal o) = o
  delete _ Empty = Empty

instance {-# OVERLAPS #-} forall e f xs. (Delete e xs, DeleteList e (f:xs) ~ (f : DeleteList e xs)) => Delete e (f ': xs) where
   delete _p (Val v) = (Val v) -- :: OneOf (f ': (DeleteList e xs))
   delete p (NoVal o) = NoVal (delete p o)
   delete _p Empty = Empty

throwMember :: forall e es m a. (Member e es, MonadError (OneOf es) m) => e -> m a
throwMember = throwError . review oneOf

liftMember :: forall e es m a. (Member e es, MonadError (OneOf es) m) => Either e a -> m a
liftMember = either throwMember return

-- | Run an action with @e@ added to the current error set @es@.
-- Typically this is used by forcing the action into ExceptT with
-- the augmented error type:
-- @@
--   catchMember withExceptT (fileIO @(FileError ': e) (Right <$> query st (LookValue key))) (return . Left)
-- @@
catchMember ::
  forall esplus e es m n a.
  (Member e esplus, es ~ DeleteList e esplus,
   MonadError (OneOf esplus) m, MonadError (OneOf es) n)
  => (forall b. (OneOf esplus -> OneOf es) -> m b -> n b)
  -> m a -> (e -> n a) -> n a
catchMember helper ma f =
  -- The idea here is that we use tryError to bring a copy of e into
  -- the return value, then we can just delete e from error monad.
  helper (delete @e Proxy) (tryError ma) >>= either handle return
  where handle :: OneOf esplus -> n a
        handle es = maybe (throwError (delete @e Proxy es)) f (get es :: Maybe e)

-- | Simplified catchMember where the monad doesn't change.
tryMember :: forall e es m a. (Member e es, MonadError (OneOf es) m) => m a -> (e -> m a) -> m a
tryMember ma f = tryError ma >>= either (\es -> maybe ma f (get es :: Maybe e)) return

-- | Annotate a member error that has been thrown.
mapMember :: forall e es m a. (Member e es, MonadError (OneOf es) m) => (e -> m e) -> m a -> m a
mapMember f ma =
  tryError ma >>= either (\es -> maybe (throwError es) (\e -> f e >>= throwMember) (get es :: Maybe e)) return

runNullExceptT :: Functor m => ExceptT (OneOf '[]) m a -> m a
runNullExceptT m = (\(Right a) -> a) <$> runExceptT m

runNullExcept :: Except (OneOf '[]) a -> a
runNullExcept m = (\(Right a) -> a) (runExcept m)

liftUIO ::
  (Unexceptional m, Member NonIOException e, Member IOException e, MonadError (OneOf e) m)
  => IO a
  -> m a
liftUIO io =
  runExceptT (fromIO io) >>= either (either throwMember throwMember . splitException) return
  where
    splitException :: SomeNonPseudoException -> Either NonIOException IOException
    splitException e = maybe (Left (NonIOException e)) Right (fromException (toException e) :: Maybe IOException)

-- | Catch any FileError thrown and put it in the return value.
runOneOf'' ::
  forall (esplus :: [*]) e (es :: [*]) m a.
  (es ~ DeleteList e esplus, Member e esplus, Monad m)
  => ExceptT (OneOf esplus) m a
  -> ExceptT (OneOf es) m (Either e a)
runOneOf'' action = catchMember withExceptT (Right <$> action) (return . Left)

runOneOf' ::
  forall (esplus :: [*]) e (es :: [*]) m a r.
  (es ~ DeleteList e esplus, Member e esplus, MonadError (OneOf es) m)
  => ExceptT (OneOf esplus) m a
  -> (Either e a -> m r)
  -> m r
runOneOf' action final = runExceptT (runOneOf'' action) >>= either throwError final

runOneOf ::
  forall (esplus :: [*]) e (es :: [*]) m a.
  (es ~ DeleteList e esplus, Member e esplus, MonadError (OneOf es) m)
  => ExceptT (OneOf esplus) m a
  -> m (Either e a)
runOneOf action = runOneOf' action return

type Errors e = (Show (OneOf e), Typeable e, HasCallStack)
type Errors' e = (Show (OneOf e), Typeable e)

-- ** Example

data ErrorFoo = Foo deriving Show
data ErrorBar = Bar deriving Show
data ErrorBaz = Baz deriving Show

type AppErrors = OneOf '[ErrorFoo, ErrorBar, ErrorBaz]

handleBar :: (Member ErrorBar errors) => OneOf errors -> IO (DeleteOneOf ErrorBar (OneOf errors))
handleBar err =
  do case get err of
       Nothing -> putStrLn "no Bar error to handle"
       (Just Bar) -> putStrLn "took care of Bar"
     pure (delete @ErrorBar Proxy err)

handleFoo :: ({-IsMember ErrorFoo errors ~ 'True,-} Get ErrorFoo errors, Delete ErrorFoo errors) => OneOf errors -> IO (DeleteOneOf ErrorFoo (OneOf errors))
handleFoo err =
  do case get err of
       Nothing -> putStrLn "no Bar error to handle"
       (Just Foo) -> putStrLn "took care of Bar"
     pure (delete @ErrorFoo Proxy err)

{-
Generates the ouput:

current error = Foo
no Bar error to handle
current error = Foo
took care of Bar
current error = {}
-}
test :: IO ()
test =
  do let errAll = set Foo :: AppErrors
     putStrLn $ "current error = " ++ show errAll

     errNoBar <- handleBar errAll
     putStrLn $ "current error = " ++ show errNoBar

     errNoFoo <- handleFoo errNoBar
     putStrLn $ "current error = " ++ show errNoFoo

     -- this will not compile because ErrorFoo has already been handled
--     errNoFoFoo <- handleFoo errNoFoo

     pure ()
