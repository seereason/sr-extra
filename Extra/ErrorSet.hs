{-# language ConstraintKinds #-}
{-# language DataKinds #-}
{-# language DeriveAnyClass #-}
{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language GADTs #-}
{-# language InstanceSigs #-}
{-# language KindSignatures #-}
{-# language MultiParamTypeClasses #-}
{-# language OverlappingInstances #-}
{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}
{-# language PolyKinds #-}
-- {-# language QuantifiedConstraints #-}
{-# language RankNTypes #-}
{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language TypeInType #-}
{-# language TypeOperators #-}
{-# language UndecidableInstances #-}

module Extra.ErrorSet
  ( IsMember
  , Nub
  , OneOf
  , Set(set)
  , Get(get)
  , follow
  , Delete(delete)
  , DeleteList
  , DeleteOneOf
  , Member
  , test
  ) where

import Control.Lens (Prism', prism')
import qualified Data.Serialize as S
import qualified Data.Serialize.Put as S
import qualified Data.Serialize.Get as S
import Data.Type.Bool
--import Data.Type.Equality
import Data.Proxy
--import GHC.Generics

type family IsMember (x :: k) (ys :: [k]) where
  IsMember x '[] = 'False
  IsMember x (x ': ys) = 'True
  IsMember x (y ': ys) = IsMember x ys

type family Nub (xs :: [k]) where
  Nub '[] = '[]
  Nub (x ': ys) = If (IsMember x ys) ys (x ': Nub ys)

-- | Since I had to flesh out my knowledge of GADTs to write the
-- Serialize instance, I thought I would put down some remarks.  A
-- GADT is like a regular data type, but you specify the signature of
-- each constructor instead of giving the field types:
--
--   data OneOf n
--     = Empty
--     | Val e
--     | NoVal o
--
-- What the regular type doesn't capture is that the result of applying
-- Val or NoVal is not OneOf s, but rather 

data OneOf (n :: [k]) :: * where
  Empty :: OneOf s
  Val   :: e -> OneOf (e ': s)
  NoVal :: OneOf s -> OneOf (e ': s)

instance Show (OneOf '[]) where
  show Empty = "{}"

instance (Show e, Show (OneOf s)) => Show (OneOf (e ': s)) where
  show (Val e) = show e
  show (NoVal o) = show o
  show Empty  = "{}"

#if 0
instance S.Serialize (OneOf '[]) where
  get :: S.Get (OneOf s)
  get = return Empty
  put :: OneOf s -> S.PutM ()
  put Empty = return ()

instance (S.Serialize e, S.Serialize (OneOf s)) => S.Serialize (OneOf (e ': s)) where
  get :: S.Get (OneOf (e ': s))
  get = do
    w <- S.getWord8
    case w of
      0 -> do
        Val <$> get
        return $ Val e
      1 -> undefined
      2 -> return Empty
  put :: OneOf (e ': s) -> S.PutM ()
  put (Val e) = undefined
  put (NoVal o) = undefined
  put Empty = undefined
#endif

instance S.Serialize (OneOf s) where
  get :: S.Get (OneOf s)
  get = undefined
  put :: OneOf s -> S.PutM ()
  put = undefined

class Set (e :: *) (xs :: [k]) where
  set :: e -> OneOf xs

instance Set e (e ': xs) where
  set e = Val e

instance (IsMember e xs ~ 'True, Set e xs) => Set e (f ': xs) where
  set e = NoVal (set e)

class Get (e :: *) (xs :: [k]) where
  get :: OneOf xs -> Maybe e

instance Get e (e ': xs) where
  get (Val e) = Just e
  get (NoVal _) = Nothing
  get Empty = Nothing

follow :: (Get e xs, Set e xs) => Prism' (OneOf xs) e
follow = prism' set get

instance (IsMember e xs ~ 'True, Get e xs) => Get e (f ': xs) where
  get (NoVal o) = get o
  get (Val _e) = Nothing
  get Empty = Nothing -- dsf: added because i have -Werror=incomplete-patterns

type family DeleteList (e :: k) (xs :: [k]) where
  DeleteList x '[] = '[]
  DeleteList x (x ': ys) = ys
  DeleteList x (y ': ys) = (y ': (DeleteList x ys))

type family DeleteOneOf (e :: k) (xs :: *) where
  DeleteOneOf x (OneOf ys) = OneOf (DeleteList x ys)

class Delete e xs where
  delete :: Proxy e -> OneOf xs -> DeleteOneOf e (OneOf xs)

instance Delete (e :: k) (e ': xs) where
  delete _ (Val _e) = Empty
  delete _ (NoVal o) = o
  delete _ Empty = Empty -- dsf: added because i have -Werror=incomplete-patterns

instance forall e f xs. (Delete e xs, DeleteList e (f:xs) ~ (f : DeleteList e xs)) => Delete e (f ': xs) where
   delete _p (Val v) = (Val v) -- :: OneOf (f ': (DeleteList e xs))
   delete p (NoVal o) = NoVal (delete p o)
   delete _p Empty = Empty

type Member (err :: *) (errors :: [*]) =
  (IsMember err errors ~ 'True, Set err errors, Get err errors, Delete err errors)

type family Member' (err :: *) (e :: *) where
  Member' err (OneOf errors) = Member err errors

#if 0
-- Uses QuantifiedConstraints
type Member err e =
  (forall errors. (e ~ OneOf errors, IsMember err errors ~ 'True, Set err errors, Get err errors, Delete err errors))
#endif

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

handleFoo :: (Member ErrorFoo errors, Get ErrorFoo errors, Delete ErrorFoo errors) => OneOf errors -> IO (DeleteOneOf ErrorFoo (OneOf errors))
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
