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
  , follow
  , DeleteList
  , DeleteOneOf
  , Delete(delete)
  , Member
  , test
  ) where

import Control.Lens (Prism', prism')
import Data.Type.Bool
import Data.Type.Equality
import Data.Word (Word8)
import Data.SafeCopy
import qualified Data.Serialize as S (Serialize(get, put), getWord8, Put, PutM, Get)
import Data.Typeable (Typeable, typeOf)
import Data.Proxy

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

instance (S.Serialize e, S.Serialize (OneOf s)) => S.Serialize (OneOf (e ': s)) where
  put :: OneOf (e ': s) -> S.PutM ()
  put (NoVal o) = S.put (0 :: Word8) >> S.put o
  put (Val e) = S.put (1 :: Word8) >> S.put e
  get :: S.Get (OneOf (e ': s))
  get = S.getWord8 >>= \case
    0 -> NoVal <$> S.get

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

instance (IsMember e xs ~ 'True, Get e xs) => Get e (f ': xs) where
  get (NoVal o) = get o
  get (Val e) = Nothing

follow :: (IsMember e xs ~ 'True, Get e xs, Set e xs) => Prism' (OneOf xs) e
follow = prism' set get

type family DeleteList e xs where
  DeleteList x '[] = '[]
  DeleteList x (x ': ys) = ys
  DeleteList x (y ': ys) = (y ': (DeleteList x ys))

type family DeleteOneOf e xs where
  DeleteOneOf x (OneOf ys) = OneOf (DeleteList x ys)

class Delete e xs where
  delete :: Proxy e -> OneOf xs -> DeleteOneOf e (OneOf xs)

instance Delete e (e ': xs) where
  delete _ (Val e) = Empty
  delete _ (NoVal o) = o
  delete _ Empty = Empty

instance {-# OVERLAPS #-} forall e f xs. (Delete e xs, DeleteList e (f:xs) ~ (f : DeleteList e xs)) => Delete e (f ': xs) where
   delete p (Val v) = (Val v) -- :: OneOf (f ': (DeleteList e xs))
   delete p (NoVal o) = NoVal (delete p o)
   delete p Empty = Empty

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

handleFoo :: (IsMember ErrorFoo errors ~ 'True, Get ErrorFoo errors, Delete ErrorFoo errors) => OneOf errors -> IO (DeleteOneOf ErrorFoo (OneOf errors))
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
