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
-- {-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -Wredundant-constraints #-}

-- TO DO:
-- * Specialize the Top instance to be invisible OR
--       (But if we have an instance
-- * Figure out how to get gshow 'x' to be "'x'" rather than unboxed stuff
-- * Add primitives
-- * Get lists to render as [1, 2, 3] and tuples as (1, 2, 3)  (T6 thru T8)
-- * Split class DoD1 into DoM1 and DoD1
-- * Figure out exactly what types might be passed to the methods
--   doD1, doC1, doS1 etc to understand how to split DoD1

module Extra.Generics.Show
  ( GShow0, gprecShows, gshowsPrec
  , GShow1, gLiftPrecShows, gLiftShowsPrec
  , GShow, gshow, gshows
  , myshow
  ) where

--import Debug.Trace
import Data.Foldable (foldl')
import Data.Functor.Classes (Show1(..))
import Data.Functor.Identity (Identity(Identity))
import Data.Proxy (Proxy(Proxy))
import GHC.Generics as G
import GHC.TypeLits
import Text.Show.Combinators (PrecShowS, {-ShowFields,-} noFields, showField, showListWith, showInfix, showApp, showCon, showRecord)
import qualified Text.Show.Combinators as Show (appendFields)

-- For primitive instances
-- import Data.Typeable (Typeable, typeOf)

-- For tests
#if !__GHCJS__
import Test.HUnit
import Language.Haskell.TH (location)
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Instances ()
#endif

-- for debug
-- import Debug.Trace
--import GHC.TypeLits

-- The K1 instance had constraint Show a, which means that every field
-- traversed must have a Show instance.  I removed this constraint,
-- and then also had to remove it from doK1.  Now I needed doK1 to Use
-- the Show instance for a set of base types and otherwise use the
-- Generic instance.  This can be done by creating a new class DoK1
-- and using overlapping instances.

-- Constraints for doing recusion into subtypes
type DoRep a = (Generic a, DoM1 Proxy (Rep a))
type DoRep1 f = (Generic1 f, DoM1 Identity (Rep1 f))

class                                       DoS1 p f               where doS1 :: forall a. p (Rd a) -> (String, String, String, Bool) -> (String,  Fixity) -> f a -> S1Result
instance DoRep a =>                         DoS1 p (K1 R a)        where doS1 p ti ci (K1 a) = doRecursion p ti ci a
instance DoRep1 f =>                        DoS1 Identity (Rec1 f) where doS1 (Identity st) ti ci (Rec1 r) = doRec1 st ti ci r
instance                                    DoS1 Identity Par1     where doS1 (Identity st) ti ci (Par1 a) = doPar1 st ti ci a
instance (Show1 f, DoS1 p g) =>             DoS1 p (f :.: g)       where doS1 p ti ci (Comp1 c) = doComp1 p ti ci c

class                                       DoFields p f           where doFields :: forall a. p (Rd a) -> (String, String, String, Bool) -> (String,  Fixity) -> f a -> [S1Result]
instance (DoFields p f, DoFields p g) =>    DoFields p (f :*: g)   where doFields p ti ci (x :*: y) = doFields p ti ci x <> doFields p ti ci y
instance (DoS1 p f, Selector s) =>          DoFields p (M1 S s f)  where doFields p ti ci (M1 x) = [doS1 p ti ci x]
instance                                    DoFields p U1          where doFields _ _ _ U1 = []

class                                       DoNamed p f            where doNamed :: forall a. p (Rd a) -> (String, String, String, Bool) -> (String,  Fixity) -> f a -> [(String, S1Result)]
instance (DoNamed p f, DoNamed p g) =>      DoNamed p (f :*: g)    where doNamed p ti ci (x :*: y) = doNamed p ti ci x <> doNamed p ti ci y
instance (Selector c, DoS1 p f) =>          DoNamed p (M1 S c f)   where doNamed p ti ci x'@(M1 x) = [(G.selName x', doS1 p ti ci x)]
instance                                    DoNamed p U1           where doNamed _ _ _ U1 = []

-- Its tempting to add the Constructor constraint here but it leads to
-- a missing constraint on an unexported GHC.Generics class.
class                                       DoConstructor p c f    where doConstructor :: forall a. p (Rd a) -> (String, String, String, Bool) -> (String,  Fixity) -> M1 C c f a -> C1Result
instance (DoFields p f, KnownSymbol s) =>   DoConstructor p ('MetaCons s y 'False) f
                                                                   where doConstructor p ti ci (M1 x) = doNormal ti ci (zip [0..] (doFields p ti ci x))
instance DoNamed p f =>   DoConstructor p ('MetaCons s y 'True) f  where doConstructor p ti ci (M1 x) = doRecord ti ci (zip [0..] (doNamed p ti ci x))

class                                       DoDatatype p d f       where doDatatype :: forall a. p (Rd a) -> (String, String, String, Bool) -> M1 D d f a -> D1Result
instance (DoD1 p f, Datatype d) =>          DoDatatype p d f       where doDatatype p ti (M1 x) = doD1 p ti x

class                                       DoD1 p f               where doD1 :: forall a. p (Rd a) -> (String, String, String, Bool) -> f a -> D1Result
instance (DoDatatype p d f, Datatype d) =>  DoD1 p (M1 D d f)      where doD1 p ti x@(M1 _) = doDatatype p ti x
instance (DoD1 p f, DoD1 p g) =>            DoD1 p (f :+: g)       where doD1 p ti (L1 x) = doD1 p ti x
                                                                         doD1 p ti (R1 y) = doD1 p ti y
instance (Constructor c, DoConstructor p c f) => DoD1 p (M1 C c f) where doD1 p ti x = doConstructor p ti (G.conName x, G.conFixity x) x
instance                                    DoD1 p V1              where doD1 _ _ v = case v of {}

class                                       DoM1 p f               where doM1 :: forall a. p (Rd a) -> f a -> D1Result
instance (DoDatatype p d f, Datatype d) =>  DoM1 p (M1 D d f)      where doM1 p x@(M1 _) = let ti = (G.datatypeName x, G.moduleName x, G.packageName x, G.isNewtype x) in doD1 p ti x

-- customization for generic Show --

-- These are needed to do the traversal
deriving instance Generic Int
deriving instance Generic Char

-- Instances for primitive types
instance {-# OVERLAPPING #-}                DoS1 p (K1 R Int)      where doS1 p ti ci (K1 a) = doLeaf p ti ci a
instance {-# OVERLAPPING #-}                DoS1 p (K1 R Char)     where doS1 p ti ci (K1 a) = doLeaf p ti ci a
instance {-# OVERLAPPING #-}                DoS1 p (K1 R String)   where doS1 p ti ci (K1 a) = doLeaf p ti ci a
instance {-# OVERLAPPING #-} DoS1 Proxy (K1 R a) =>
                                            DoS1 p (K1 R [a])      where doS1 p ti ci (K1 xs) = doList p ti ci (fmap myshows xs)
instance {-# OVERLAPPING #-}                DoS1 p (K1 R ())       where doS1 p ti ci (K1 ()) = doTuple p ti ci []
instance {-# OVERLAPPING #-} (DoS1 Proxy (K1 R a),
                              DoS1 Proxy (K1 R b)) =>
                                            DoS1 p (K1 R (a, b))   where doS1 p ti ci (K1 (a, b)) = doTuple p ti ci [myshows a, myshows b]
instance {-# OVERLAPPING #-} (DoS1 Proxy (K1 R a),
                              DoS1 Proxy (K1 R b),
                              DoS1 Proxy (K1 R c)) =>
                                            DoS1 p (K1 R (a, b, c))
                                                                   where doS1 p ti ci (K1 (a, b, c)) = doTuple p ti ci [myshows a, myshows b, myshows c]
instance {-# OVERLAPPING #-} (DoS1 Proxy (K1 R a),
                              DoS1 Proxy (K1 R b),
                              DoS1 Proxy (K1 R c),
                              DoS1 Proxy (K1 R d)) =>
                                            DoS1 p (K1 R (a, b, c, d))
                                                                   where doS1 p ti ci (K1 (a, b, c, d)) = doTuple p ti ci [myshows a, myshows b, myshows c, myshows d]
instance {-# OVERLAPPING #-} (DoS1 Proxy (K1 R a),
                              DoS1 Proxy (K1 R b),
                              DoS1 Proxy (K1 R c),
                              DoS1 Proxy (K1 R d),
                              DoS1 Proxy (K1 R e)) =>
                                            DoS1 p (K1 R (a, b, c, d, e))
                                                                   where doS1 p ti ci (K1 (a, b, c, d, e)) = doTuple p ti ci [myshows a, myshows b, myshows c, myshows d, myshows e]

-- Instances for unboxed types
instance                                    DoS1 Proxy (URec Int)  where doS1 p ti ci a = doUnboxed p ti ci a
instance                                    DoS1 Proxy (URec Char) where doS1 p ti ci a = doUnboxed p ti ci a

type Rd a = (Int -> a -> ShowS, [a] -> ShowS) -- Like a Reader monad
type S1Result = PrecShowS -- The result of a single field of a constructor
type C1Result = PrecShowS -- Result of processing one constructors
type D1Result = PrecShowS -- Result of processing a type value

doLeaf :: Show a => p -> (String, String, String, Bool) -> (String, Fixity) -> a -> S1Result
doLeaf _p _ti _ci a _prec = shows a

doList :: p -> (String, String, String, Bool) -> (String, Fixity) -> [ShowS] -> S1Result
doList _ _ _ [] = \_ -> showChar '[' . showChar ']'
doList _ _ _ xs = \_ -> showChar '[' . foldl1 (\a b -> a . showString "," . b) xs . showChar ']'

doTuple :: p -> (String, String, String, Bool) -> (String, Fixity) -> [ShowS] -> S1Result
doTuple _ _ _ [] = \_ -> showString "()"
doTuple _ _ _ ks = \_ -> showChar '(' . foldl1 (\a b -> a . showString "," . b) ks . showChar ')'

doUnboxed :: Show a => p -> (String, String, String, Bool) -> (String, Fixity) -> a -> S1Result
doUnboxed _p _ti _ci a _prec s = show a <> s

doRecursion :: (Generic a, DoM1 Proxy (Rep a)) => p -> (String, String, String, Bool) -> (String,  Fixity) -> a -> S1Result
doRecursion _p _ti _ci a = flip gshowsPrec a

-- Rec1 marks a field (S1) which is itself a record
doRec1 :: (Generic1 f, DoM1 Identity (Rep1 f)) => forall a. Rd a -> (String, String, String, Bool) -> (String,  Fixity) -> f a -> S1Result
doRec1 sp _ti _ci r = flip (uncurry gLiftShowsPrec sp) r

-- Par1 marks a field (S1) which is just a type parameter
doPar1 :: Rd a -> (String, String, String, Bool) -> (String,  Fixity) -> a -> S1Result
doPar1 (op1', _) _ti _ci a = flip op1' a

-- Comp1 is the marks a field (S1) of type (:.:), composition
doComp1 :: (Show1 f, DoS1 p g) => p (Rd a) -> (String, String, String, Bool) -> (String,  Fixity) -> f (g a) -> S1Result
doComp1 p ti ci c = flip (liftShowsPrec (flip (doS1 p ti ci)) (showListWith (flip (doS1 p ti ci) 0))) c

-- Handle the unnamed fields of a constructor (C1)
doNormal :: (String, String, String, Bool) -> (String, Fixity) -> [(Int, S1Result)] -> C1Result
doNormal _ (name, Infix _ fy) (k1 : k2 : ks) = foldl' showApp (showInfix name fy (snd k1) (snd k2)) (fmap snd ks)
doNormal _ (name, Infix _ _) ks =              foldl' showApp (showCon ("(" ++ name ++ ")")) (fmap snd ks)
doNormal _ (name, Prefix) ks =                   foldl' showApp (showCon         name        ) (fmap snd ks)

-- Handle the named fields of a constructor (C1)
doRecord :: (String, String, String, Bool) -> (String, Fixity) -> [(Int, (String, S1Result))] -> C1Result
doRecord _ (cname, _) [] = showRecord cname noFields
-- Do not render the Top type, it is private to this module and is
-- only used to get the recursion to work.
doRecord ("Top", "Extra.Generics.Show", _, _) _ [(_, (_, r))] = r
doRecord _ (cname, Prefix) ks = showRecord cname (foldl1 Show.appendFields (fmap (uncurry showField) (fmap snd ks)))
doRecord _ (cname, Infix _ _) ks = showRecord ("(" ++ cname ++ ")") (foldl1 Show.appendFields (fmap (uncurry showField) (fmap snd ks)))

---------------------------------------------------

-- | Generic representation of 'Show' types.
type GShow0 = DoM1 Proxy

newtype Top a = Top {unTop :: a} deriving Generic
infixr 0 `Top`

-- | Generic 'showsPrec'.
--
-- @
-- instance 'Show' MyType where
--   'showsPrec' = 'gshowsPrec'
-- @
gprecShows :: forall a. (Generic a, GShow0 (Rep a)) => a -> PrecShowS
gprecShows = doM1 (Proxy :: Proxy (Rd a)) . from

gshowsPrec :: (Generic a, GShow0 (Rep a)) => Int -> a -> ShowS
gshowsPrec = flip gprecShows

-- | Generic representation of 'Data.Functor.Classes.Show1' types.
-- class Show1 (f :: * -> *) where
--   liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> f a -> ShowS
--   liftShowsList :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> [f a] -> ShowS
type GShow1 = DoM1 Identity

gLiftPrecShows ::
  (GShow1 f)
  => (Int -> a -> ShowS)
  -> ([a] -> ShowS)
  -> f a -> PrecShowS
gLiftPrecShows = curry (doM1 . Identity)

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


type GShow a = (Generic a, DoM1 Proxy (Rep a))

gshow :: GShow a => a -> String
gshow x = gshows x ""
gshows :: GShow a => a -> ShowS
gshows = gshowsPrec 0

myshow :: (DoS1 Proxy (K1 R a)) => a -> String
myshow a = gshow (Top a)

myshows :: (DoS1 Proxy (K1 R a)) => a -> ShowS
myshows a = gshows (Top a)

#if !__GHCJS__
_tests :: IO ()
_tests = do
  r@Counts{..} <-
    runTestTT
      (TestList
       [ TestCase (assertEqual "T1" "Foo {n = 1, ch = 'x'}" (myshow (Foo 1 'x')))
       , TestCase (assertEqual "T2" "Bar [Foo {n = 1, ch = 'x'}] \"hello\"" (myshow (Bar [Foo 1 'x'] "hello")))
       , TestCase (assertEqual "T3" "Fork 'a' [Fork 'b' []]" (myshow (Fork 'a' [Fork 'b' []])))
       , TestCase (assertEqual "T4" "Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))" (myshow (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c')))))
       , TestCase (assertEqual "T5" "WithInt 5 (WithInt 2 'a')" (myshow (WithInt 5 (WithInt 2 'a'))))
       , let (expected, loc) = $(lift =<< (\x -> (show x, x)) <$> location) in
           TestCase (assertEqual "T6" expected (myshow loc))
       , TestCase (assertEqual "T7" "[1,2,3]" (myshow ([1,2,3] :: [Int])))
       , TestCase (assertEqual "T8" "[1]" (myshow ([1] :: [Int])))
       , TestCase (assertEqual "T9" "1" (myshow (1 :: Int)))
       , TestCase (assertEqual "T10" "('x',2,\"abc\")" (myshow ('x',(2 :: Int),("abc" :: String))))
       ])
  case (errors, failures) of
    (0, 0) -> return ()
    _ -> error (show r)
#endif
