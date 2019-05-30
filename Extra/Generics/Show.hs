{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -Wall -Wredundant-constraints #-}

module Extra.Generics.Show
  ( GShow0, gprecShows, gshowsPrec
  , GShow1, gLiftPrecShows, gLiftShowsPrec
  , GShow, gshow, gshows
  ) where

import Data.Foldable (foldl')
import Data.Functor.Classes (Show1(..))
import Data.Functor.Identity (Identity(Identity))
import Data.Proxy (Proxy(Proxy))
import GHC.Generics (Generic,
                     Generic1,
                     Rep,
                     Rep1,
                     from,
                     from1,
                     -- Meta info
                     M1(M1), D, C, S,  -- S1 = M1 S, C1 = M1 C, D1 = M1 D
                     V1, -- void datatype, no constructors
                     (:+:)(L1, R1), -- datatype constructors
                     (:*:)((:*:)), -- constructor fields
                     (:.:)(Comp1),
                     K1(K1), -- constructor fields
                     U1(U1), -- constructors without arguments
                     Constructor, -- Class of datatypes that represent constructors
                     conFixity,
                     conName,
                     Fixity(Infix, Prefix),
                     Selector, -- Class of datatypes that represent records

                     Meta(MetaCons),
                     Par1(Par1), -- Marks a field which is just a type parameter
                     Rec1(Rec1), -- Marks a field which is itself a record
                     selName)
import Text.Show.Combinators (PrecShowS, {-ShowFields,-} noFields, showField, showListWith, showInfix, showApp, showCon, showRecord)
import qualified Text.Show.Combinators as Show (appendFields)

class                                    DoS1 p f               where doS1 :: p (Rd a) -> f a -> S1Result
instance Show a                       => DoS1 p (K1 i a)        where doS1 _ (K1 x) = doK1 x
instance Show1 f                      => DoS1 Identity (Rec1 f) where doS1 (Identity st) (Rec1 r) = doRec1 st r
instance                                 DoS1 Identity Par1     where doS1 (Identity st) (Par1 a) = doPar1 st a
instance (Show1 f, DoS1 p g)          => DoS1 p (f :.: g)       where doS1 p (Comp1 c) = doComp1 p c

class                                    DoFields p f           where doFields :: p (Rd a) -> f a -> FieldsResult
instance (DoFields p f, DoFields p g) => DoFields p (f :*: g)   where doFields p (x :*: y) = appendFields (doFields p x) (doFields p y)
instance DoS1 p f                     => DoFields p (M1 S c f)  where doFields p (M1 x) = [doS1 p x]
instance                                 DoFields p U1          where doFields _ U1 = []

class                                    DoNamed p f            where doNamed :: p (Rd a) -> f a -> NamedResult
instance (DoNamed p f, DoNamed p g)   => DoNamed p (f :*: g)    where doNamed p (x :*: y) = appendNamed (doNamed p x) (doNamed p y)
instance (Selector c, DoS1 p f)       => DoNamed p (M1 S c f)   where doNamed p x'@(M1 x) = doNamedField x' (doS1 p x)
instance                                 DoNamed p U1           where doNamed _ U1 = noFields

class                    DoC1 p c f                             where doC1 :: p (Rd a) -> String -> Fixity -> M1 C c f a -> C1Result
instance DoFields p f => DoC1 p ('MetaCons s y 'False) f        where doC1 p name fixity (M1 x) = doNormal name fixity (doFields p x)
instance DoNamed p f  => DoC1 p ('MetaCons s y 'True) f         where doC1 p name fixity (M1 x) = doRecord name fixity (doNamed p x)

class                                    DoD1 p f               where doD1 :: p (Rd a) -> f a -> D1Result
instance DoD1 p f                     => DoD1 p (M1 D d f)      where doD1 p (M1 x) = doD1 p x
instance (DoD1 p f, DoD1 p g)         => DoD1 p (f :+: g)       where doD1 p (L1 x) = doD1 p x
                                                                      doD1 p (R1 y) = doD1 p y
instance (Constructor c, DoC1 p c f)  => DoD1 p (M1 C c f)      where doD1 p x = doC1 p (conName x) (conFixity x) x
instance                                 DoD1 p V1              where doD1 _ v = case v of {}

-- customization --

type Rd a = (Int -> a -> ShowS, [a] -> ShowS) -- ShowsPrec
type S1Result = PrecShowS
type FieldsResult = [PrecShowS]
type NamedResult = ShowS
type C1Result = PrecShowS
type D1Result = PrecShowS

doK1 :: Show a => a -> S1Result
doK1 = flip showsPrec

doRec1 :: Show1 f => Rd a -> f a -> S1Result
doRec1 sp r = flip (uncurry liftShowsPrec sp) r

doPar1 :: Rd a -> a -> S1Result
doPar1 (op1', _) a = flip op1' a

doComp1 :: (Show1 f, DoS1 p g) => p (Rd a) -> f (g a) -> S1Result
doComp1 p c = flip (liftShowsPrec (flip (doS1 p)) (showListWith (flip (doS1 p) 0))) c

appendFields :: FieldsResult -> FieldsResult -> FieldsResult
appendFields = mappend

appendNamed :: NamedResult -> NamedResult -> NamedResult
appendNamed = Show.appendFields

doNormal :: String -> Fixity -> FieldsResult -> C1Result
doNormal name (Infix _ fy) (k1 : k2 : ks) = foldl' showApp (showInfix name fy k1 k2)      ks
doNormal name (Infix _ _) ks =              foldl' showApp (showCon ("(" ++ name ++ ")")) ks
doNormal name Prefix ks =                   foldl' showApp (showCon         name        ) ks

doRecord :: String -> Fixity -> NamedResult -> C1Result
doRecord name Prefix r = showRecord name r
doRecord name (Infix _ _) r = showRecord ("(" ++ name ++ ")") r

doNamedField :: Selector c => M1 S c f a -> S1Result -> NamedResult
doNamedField s r = showField (selName s) r

-- liftOps :: Show1 (f :: * -> *) => (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> f a -> ShowS
-- liftOps = liftShowsPrec

---------------------------------------------------

-- | Generic representation of 'Show' types.
type GShow0 = DoD1 Proxy

-- | Generic 'showsPrec'.
--
-- @
-- instance 'Show' MyType where
--   'showsPrec' = 'gshowsPrec'
-- @
gprecShows :: (Generic a, GShow0 (Rep a)) => a -> PrecShowS
gprecShows = doD1 Proxy . from

gshowsPrec :: (Generic a, GShow0 (Rep a)) => Int -> a -> ShowS
gshowsPrec = flip gprecShows

-- | Generic representation of 'Data.Functor.Classes.Show1' types.
-- class Show1 (f :: * -> *) where
--   liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> f a -> ShowS
--   liftShowsList :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> [f a] -> ShowS
type GShow1 = DoD1 Identity

gLiftPrecShows ::
  GShow1 f
  => (Int -> a -> ShowS)
  -> ([a] -> ShowS)
  -> f a -> PrecShowS
gLiftPrecShows = curry (doD1 . Identity)

-- | Generic 'liftShowsPrec'.
gLiftShowsPrec ::
  (Generic1 f, GShow1 (Rep1 f))
  => (Int -> a -> ShowS)
  -> ([a] -> ShowS)
  -> Int
  -> f a -> ShowS
gLiftShowsPrec op1' op2' =
  flip (gLiftPrecShows op1' op2' . from1)

data Foo = Foo {n :: Int, ch :: Char} deriving (Generic, Show)
data Bar = Bar [Foo] String deriving (Generic, Show)
data Rose a = Fork a [Rose a] deriving (Generic, Show)
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Generic, Show)
data WithInt a = WithInt Int a deriving (Generic, Show)

type GShow a = (Generic a, DoD1 Proxy (Rep a))

gshow :: GShow a => a -> String
gshow x = gshows x ""
gshows :: GShow a => a -> ShowS
gshows = gshowsPrec 0

_test :: IO ()
_test = do
  putStrLn (gshow (Foo 1 'x'))
  putStrLn (gshow (Bar [Foo 1 'x'] "hello"))
  putStrLn (gshow (Fork 'a'[Fork 'b' []]))
  putStrLn (gshow (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))))
  putStrLn (gshow (WithInt 5 (WithInt 2 'a')))
