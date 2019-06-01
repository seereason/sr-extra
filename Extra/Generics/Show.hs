-- | This is a both a working implementation of Generic Show and a
-- template for using GHC.Generics to implement similar functions
-- without requiring that every type it can operate on have a Show
-- instance.  It does require an instance for each base type.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -Wredundant-constraints #-}

module Extra.Generics.Show
  ( GShow0, gprecShows, gshowsPrec
  , GShow1, gLiftPrecShows, gLiftShowsPrec
  , GShow, gshow, gshows
  ) where

--import Debug.Trace
import Data.Foldable (foldl')
import Data.Functor.Classes (Show1(..))
import Data.Functor.Identity (Identity(Identity))
import Data.Proxy (Proxy(Proxy))
import GHC.Generics as G
import Test.HUnit
import Text.Show.Combinators (PrecShowS, {-ShowFields,-} noFields, showField, showListWith, showInfix, showApp, showCon, showRecord)
import qualified Text.Show.Combinators as Show (appendFields)

-- For primitive instances
import Data.Typeable (Typeable, typeRep)

-- For tests
import Language.Haskell.TH (location)
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Instances ()

-- for debug
import Debug.Trace
import GHC.TypeLits

-- The K1 instance had constraint Show a, which means that every field
-- traversed must have a Show instance.  I removed this constraint,
-- and then also had to remove it from doK1.  Now I needed doK1 to Use
-- the Show instance for a set of base types and otherwise use the
-- Generic instance.  This can be done by creating a new class DoK1
-- and using overlapping instances.

-- Constraints for doing recusion into subtypes
type DoRep1 f = (Generic1 f, DoD1 Identity (Rep1 f))
type DoRep a = (Generic a, DoD1 Proxy (Rep a))

class                                       DoS1 p f               where doS1 :: p (Rd a) -> f a -> S1Result
instance DoRep a =>                         DoS1 p (K1 R a)        where doS1 p (K1 a) = doRecursion p a
instance DoRep1 f =>                        DoS1 Identity (Rec1 f) where doS1 (Identity st) (Rec1 r) = doRec1 st r
instance                                    DoS1 Identity Par1     where doS1 (Identity st) (Par1 a) = doPar1 st a
instance (Show1 f, DoS1 p g) =>             DoS1 p (f :.: g)       where doS1 p (Comp1 c) = doComp1 p c

class                                       DoFields p f           where doFields :: p (Rd a) -> f a -> [S1Result]
instance (DoFields p f, DoFields p g) =>    DoFields p (f :*: g)   where doFields p (x :*: y) = doFields p x <> doFields p y
instance DoS1 p f =>                        DoFields p (M1 S c f)  where doFields p (M1 x) = [doS1 p x]
instance                                    DoFields p U1          where doFields _ U1 = []

class                                       DoNamed p f            where doNamed :: p (Rd a) -> f a -> [(String, S1Result)]
instance (DoNamed p f, DoNamed p g) =>      DoNamed p (f :*: g)    where doNamed p (x :*: y) = doNamed p x <> doNamed p y
instance (Selector c, DoS1 p f) =>          DoNamed p (M1 S c f)   where doNamed p x'@(M1 x) = [(G.selName x', doS1 p x)]
instance                                    DoNamed p U1           where doNamed _ U1 = []

class                                       DoC1 p c f             where doC1 :: p (Rd a) -> String -> Fixity -> M1 C c f a -> C1Result
instance DoFields p f =>           DoC1 p ('MetaCons s y 'False) f where doC1 p name fixity (M1 x) = doNormal name fixity (zip [0..] (doFields p x))
instance DoNamed p f =>            DoC1 p ('MetaCons s y 'True) f  where doC1 p name fixity (M1 x) = doRecord name fixity (zip [0..] (doNamed p x))

class                                       DoD1 p f               where doD1 :: p (Rd a) -> f a -> D1Result
instance (Datatype d, DoD1 p f) =>          DoD1 p (M1 D d f)      where doD1 p (M1 x) = doD1 p x
instance (DoD1 p f, DoD1 p g) =>            DoD1 p (f :+: g)       where doD1 p (L1 x) = doD1 p x
                                                                         doD1 p (R1 y) = doD1 p y
instance (Constructor c, DoC1 p c f)  =>    DoD1 p (M1 C c f)      where doD1 p x = doC1 p (G.conName x) (G.conFixity x) x
instance                                    DoD1 p V1              where doD1 _ v = case v of {}

{-
class                                       DoM1 p f               where doM1 :: p (Rd a) -> f a -> D1Result
instance (Datatype d, DoD1 p f) =>          DoM1 p (M1 D d f)      where doM1 p (M1 x) = doD1 p (G.datatypeName x) (G.moduleName x) (G.packageName x) (G.isNewtype x) x
-}

-- customization for generic Show --

-- These are needed to do the traversal
deriving instance Generic Int
deriving instance Generic Char

-- Instances for primitive types
instance {-# OVERLAPPING #-}                DoS1 p (K1 R Int)      where doS1 p (K1 a) = doLeaf p a
instance {-# OVERLAPPING #-}                DoS1 p (K1 R Char)     where doS1 p (K1 a) = doLeaf p a
instance {-# OVERLAPPING #-}                DoS1 p (K1 R String)   where doS1 p (K1 a) = doLeaf p a
instance {-# OVERLAPPING #-} GShow a =>     DoS1 p (K1 R (Top a))  where doS1 p (K1 a) = doTop p a

-- Instances for unboxed types
instance                                    DoS1 Proxy (URec Int)  where doS1 p a = doUnboxed p a
instance                                    DoS1 Proxy (URec Char) where doS1 p a = doUnboxed p a

type Rd a = (Int -> a -> ShowS, [a] -> ShowS) -- Like a Reader monad
type S1Result = PrecShowS -- The result of a single field of a constructor
type C1Result = PrecShowS -- Result of processing one constructors
type D1Result = PrecShowS -- Result of processing a type value

doTop :: GShow a => p -> Top a -> S1Result
doTop _p (Top a) _prec s = gshow a <> s

doLeaf :: Show a => p -> a -> S1Result
doLeaf _p a _prec s = show a <> s

doUnboxed :: Show a => p -> a -> S1Result
doUnboxed _p a _prec s = show a <> s

doRecursion :: (Generic a, DoD1 Proxy (Rep a)) => p -> a -> S1Result
doRecursion _p a = flip gshowsPrec a

-- Rec1 marks a field (S1) which is itself a record
doRec1 :: (Generic1 f, DoD1 Identity (Rep1 f)) => Rd a -> f a -> S1Result
doRec1 sp r = flip (uncurry gLiftShowsPrec sp) r

-- Par1 marks a field (S1) which is just a type parameter
doPar1 :: Rd a -> a -> S1Result
doPar1 (op1', _) a = flip op1' a

-- Comp1 is the marks a field (S1) of type (:.:), composition
doComp1 :: (Show1 f, DoS1 p g) => p (Rd a) -> f (g a) -> S1Result
doComp1 p c = flip (liftShowsPrec (flip (doS1 p)) (showListWith (flip (doS1 p) 0))) c

-- Handle the unnamed fields of a constructor (C1)
doNormal :: String -> Fixity -> [(Int, S1Result)] -> C1Result
doNormal name (Infix _ fy) (k1 : k2 : ks) = foldl' showApp (showInfix name fy (snd k1) (snd k2)) (fmap snd ks)
doNormal name (Infix _ _) ks =              foldl' showApp (showCon ("(" ++ name ++ ")")) (fmap snd ks)
doNormal name Prefix ks =                   foldl' showApp (showCon         name        ) (fmap snd ks)

-- Handle the named fields of a constructor (C1)
doRecord :: String -> Fixity -> [(Int, (String, S1Result))] -> C1Result
doRecord cname _ [] = showRecord cname noFields
doRecord cname Prefix ks = showRecord cname (foldl1 Show.appendFields (fmap (uncurry showField) (fmap snd ks)))
doRecord cname (Infix _ _) ks = showRecord ("(" ++ cname ++ ")") (foldl1 Show.appendFields (fmap (uncurry showField) (fmap snd ks)))

---------------------------------------------------

-- | Generic representation of 'Show' types.
type GShow0 = DoD1 Proxy

newtype Top a = Top a deriving Generic
infixr 0 `Top`

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

myshow a = gshow (Top a)

_tests :: IO ()
_tests = do
  r@Counts{..} <-
    runTestTT
      (TestList
       [ TestCase (assertEqual "T1" "Foo {n = 1, ch = 'x'}" (myshow (Foo 1 'x')))
       , TestCase (assertEqual "T2" "Bar (Foo {n = 1, ch = 'x'} : []) \"hello\"" (myshow (Bar [Foo 1 'x'] "hello")))
       , TestCase (assertEqual "T3" "Fork 'a' (Fork 'b' [] : [])" (myshow (Fork 'a'[Fork 'b' []])))
       , TestCase (assertEqual "T4" "Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))" (myshow (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c')))))
       , TestCase (assertEqual "T5" "WithInt 5 (WithInt 2 'a')" (myshow (WithInt 5 (WithInt 2 'a'))))
       , let (expected, loc) = $(lift =<< (\x -> (show x, x)) <$> location) in
           TestCase (assertEqual "T6" expected (myshow loc))
       , TestCase (assertEqual "T7" "[1,2,3]" (myshow ([1,2,3] :: [Int])))
       , TestCase (assertEqual "T8" "[1]" (myshow ([1] :: [Int])))
       , TestCase (assertEqual "T9" "1" (myshow (1 :: Int)))
       ])
  case (errors, failures) of
    (0, 0) -> return ()
    _ -> error (show r)
