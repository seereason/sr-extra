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

module Extra.Generics.Serialize where

--import Debug.Trace
import Data.Foldable (foldl')
import Data.Functor.Classes (Show1(..))
import Data.Functor.Identity (Identity(Identity))
import Data.Proxy (Proxy(Proxy))
import Generic.Data (gconIndex)
import GHC.Generics as G
import GHC.TypeLits
import Text.Show.Combinators (PrecShowS, {-ShowFields,-} noFields, showField, showListWith, showInfix, showApp, showCon, showRecord)
import qualified Text.Show.Combinators as Show (appendFields)

-- For customization
import Data.Binary.Generic
import Data.ByteString (ByteString, singleton)
import Data.Word (Word8)
import Data.Serialize (encode, Serialize)

-- For primitive instances
-- import Data.Typeable (Typeable, typeOf)

-- For tests
#if !__GHCJS__
import Test.HUnit
import Language.Haskell.TH (location)
import Language.Haskell.TH.Lift (lift)
import Language.Haskell.TH.Syntax (Loc)
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

class                                       DoS1 p f               where doS1 :: forall a. p (Rd a) -> (String, String, String, Bool) -> (Int, String,  Fixity) -> f a -> S1Result
instance {-# OVERLAPPABLE #-} DoRep a =>    DoS1 p (K1 R a)        where doS1 p ti ci (K1 a) = doRecursion p ti ci a
-- instance DoRep1 f =>                        DoS1 Identity (Rec1 f) where doS1 (Identity st) ti ci (Rec1 r) = doRec1 st ti ci r
-- instance                                    DoS1 Identity Par1     where doS1 (Identity st) ti ci (Par1 a) = doPar1 st ti ci a
--instance (Show1 f, DoS1 p g) =>             DoS1 p (f :.: g)       where doS1 p ti ci (Comp1 c) = doComp1 p ti ci c

class                                       DoFields p f           where doFields :: forall a. p (Rd a) -> (String, String, String, Bool) -> (Int, String,  Fixity) -> f a -> [S1Result]
instance (DoFields p f, DoFields p g) =>    DoFields p (f :*: g)   where doFields p ti ci (x :*: y) = doFields p ti ci x <> doFields p ti ci y
instance (DoS1 p f, Selector s) =>          DoFields p (M1 S s f)  where doFields p ti ci (M1 x) = [doS1 p ti ci x]
instance                                    DoFields p U1          where doFields _ _ _ U1 = []

class                                       DoNamed p f            where doNamed :: forall a. p (Rd a) -> (String, String, String, Bool) -> (Int, String,  Fixity) -> f a -> [(String, S1Result)]
instance (DoNamed p f, DoNamed p g) =>      DoNamed p (f :*: g)    where doNamed p ti ci (x :*: y) = doNamed p ti ci x <> doNamed p ti ci y
instance (Selector c, DoS1 p f) =>          DoNamed p (M1 S c f)   where doNamed p ti ci x'@(M1 x) = [(G.selName x', doS1 p ti ci x)]
instance                                    DoNamed p U1           where doNamed _ _ _ U1 = []

-- Its tempting to add the Constructor constraint here but it leads to
-- a missing constraint on an unexported GHC.Generics class.
class                                       DoConstructor p c f    where doConstructor :: forall a. p (Rd a) -> (String, String, String, Bool) -> (Int, String,  Fixity) -> M1 C c f a -> C1Result
instance (DoFields p f, KnownSymbol s) =>   DoConstructor p ('MetaCons s y 'False) f
                                                                   where doConstructor p ti ci (M1 x) = doNormal ti ci (zip [0..] (doFields p ti ci x))
instance DoNamed p f =>   DoConstructor p ('MetaCons s y 'True) f  where doConstructor p ti ci (M1 x) = doRecord ti ci (zip [0..] (doNamed p ti ci x))

class                                       DoDatatype p d f       where doDatatype :: forall a. p (Rd a) -> (String, String, String, Bool) -> M1 D d f a -> D1Result
instance (DoD1 p f, Datatype d) =>          DoDatatype p d f       where doDatatype p ti (M1 x) = doD1 p ti x

class                                       DoD1 p f               where doD1 :: forall a. p (Rd a) -> (String, String, String, Bool) -> f a -> D1Result
instance (DoDatatype p d f, Datatype d) =>  DoD1 p (M1 D d f)      where doD1 p ti x@(M1 _) = doDatatype p ti x
instance (DoD1 p f, DoD1 p g) =>            DoD1 p (f :+: g)       where doD1 p ti (L1 x) = doD1 p ti x
                                                                         doD1 p ti (R1 y) = doD1 p ti y
instance (Constructor c, DoConstructor p c f) => DoD1 p (M1 C c f) where doD1 p ti x = doConstructor p ti (gconIndex x, G.conName x, G.conFixity x) x
instance                                    DoD1 p V1              where doD1 _ _ v = case v of {}

class                                       DoM1 p f               where doM1 :: forall a. p (Rd a) -> f a -> D1Result
instance (DoDatatype p d f, Datatype d) =>  DoM1 p (M1 D d f)      where doM1 p x@(M1 _) = let ti = (G.datatypeName x, G.moduleName x, G.packageName x, G.isNewtype x) in doD1 p ti x

-- customization for generic Show --

-- Instances for primitive types
instance                      DoS1 p (K1 R Int)      where doS1 p ti ci (K1 a) = doLeaf p ti ci a
instance                      DoS1 p (K1 R Char)     where doS1 p ti ci (K1 a) = doLeaf p ti ci a
instance {-# OVERLAPPING #-}  DoS1 p (K1 R String)   where doS1 p ti ci (K1 a) = doLeaf p ti ci a -- overlaps [a]
instance DoS1 Proxy (K1 R a) =>
                              DoS1 p (K1 R [a])      where doS1 p ti ci (K1 xs) = doList p ti ci (fmap myencode xs)
instance                      DoS1 p (K1 R ())       where doS1 p ti ci (K1 ()) = doTuple p ti ci []
instance (DoS1 Proxy (K1 R a),
          DoS1 Proxy (K1 R b)) => DoS1 p (K1 R (a, b))
                                                     where doS1 p ti ci (K1 (a, b)) = doTuple p ti ci [myencode a, myencode b]
instance (DoS1 Proxy (K1 R a),
          DoS1 Proxy (K1 R b),
          DoS1 Proxy (K1 R c)) => DoS1 p (K1 R (a, b, c))
                                                     where doS1 p ti ci (K1 (a, b, c)) = doTuple p ti ci [myencode a, myencode b, myencode c]
instance (DoS1 Proxy (K1 R a),
          DoS1 Proxy (K1 R b),
          DoS1 Proxy (K1 R c),
          DoS1 Proxy (K1 R d)) => DoS1 p (K1 R (a, b, c, d))
                                                     where doS1 p ti ci (K1 (a, b, c, d)) = doTuple p ti ci [myencode a, myencode b, myencode c, myencode d]
instance (DoS1 Proxy (K1 R a),
          DoS1 Proxy (K1 R b),
          DoS1 Proxy (K1 R c),
          DoS1 Proxy (K1 R d),
          DoS1 Proxy (K1 R e)) => DoS1 p (K1 R (a, b, c, d, e))
                                                     where doS1 p ti ci (K1 (a, b, c, d, e)) = doTuple p ti ci [myencode a, myencode b, myencode c, myencode d, myencode e]

type Rd a = ({-Int -> a -> ShowS, [a] -> ShowS-}) -- Like a Reader monad
type S1Result = ByteString -- The result of a single field of a constructor
type C1Result = ByteString -- Result of processing one constructors
type D1Result = ByteString -- Result of processing a type value

doLeaf :: Serialize a => p -> (String, String, String, Bool) -> (Int, String, Fixity) -> a -> S1Result
doLeaf _p _ti _ci a = encode a

doList :: p -> (String, String, String, Bool) -> (Int, String, Fixity) -> [ByteString] -> S1Result
doList _ _ _ [] = mempty
doList _ _ _ xs = mconcat xs

doTuple :: p -> (String, String, String, Bool) -> (Int, String, Fixity) -> [ByteString] -> S1Result
doTuple _ _ _ [] = mempty
doTuple _ _ _ xs = mconcat xs

doRecursion :: (Generic a, DoM1 Proxy (Rep a)) => p -> (String, String, String, Bool) -> (Int, String,  Fixity) -> a -> S1Result
doRecursion _p _ti _ci a = gencode a

-- Comp1 is the marks a field (S1) of type (:.:), composition
--doComp1 :: (Show1 f, DoS1 p g) => p (Rd a) -> (String, String, String, Bool) -> (String,  Fixity) -> f (g a) -> S1Result
--doComp1 p ti ci c = flip (liftShowsPrec (flip (doS1 p ti ci)) (showListWith (flip (doS1 p ti ci) 0))) c

-- Handle the unnamed fields of a constructor (C1)
doNormal :: (String, String, String, Bool) -> (Int, String, Fixity) -> [(Int, S1Result)] -> C1Result
doNormal _ (index, name, Prefix) ks = singleton (fromIntegral index :: Word8) <> mconcat (fmap snd ks)

-- Handle the named fields of a constructor (C1)
doRecord :: (String, String, String, Bool) -> (Int, String, Fixity) -> [(Int, (String, S1Result))] -> C1Result
doRecord _ (index, cname, _) ks = singleton (fromIntegral index :: Word8) <> mconcat (fmap (snd . snd) ks)

---------------------------------------------------

-- | Generic representation of 'Show' types.
type GSerialize0 = DoM1 Proxy

type GSerialize a = (Generic a, GSerialize0 (Rep a))

-- | Generic 'showsPrec'.
--
-- @
-- instance 'Show' MyType where
--   'showsPrec' = 'gshowsPrec'
-- @
gencode :: forall a. GSerialize a => a -> ByteString
gencode = doM1 (Proxy :: Proxy (Rd a)) . from

-- | Generic representation of 'Data.Functor.Classes.Show1' types.
-- class Show1 (f :: * -> *) where
--   liftShowsPrec :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> Int -> f a -> ShowS
--   liftShowsList :: (Int -> a -> ShowS) -> ([a] -> ShowS) -> [f a] -> ShowS

-- A newtype will not change the encoding, and it helps the signature
-- of myencode.
newtype Top a = Top {unTop :: a} deriving Generic
infixr 0 `Top`

myencode :: (DoS1 Proxy (K1 R a)) => a -> ByteString
myencode a = gencode (Top a)

#if !__GHCJS__
data Foo = Foo {n :: Int, ch :: Char} deriving (Generic, Show, Serialize)
data Bar = Bar [Foo] String deriving (Generic, Show, Serialize)
data Rose a = Fork a [Rose a] deriving (Generic, Show, Serialize)
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Generic, Show, Serialize)
data WithInt a = WithInt Int a deriving (Generic, Show, Serialize)

deriving instance Serialize Loc

_tests :: IO ()
_tests = do
  r@Counts{..} <-
    runTestTT
      (TestList
       [ let x = (Foo 1 'x') in TestCase (assertEqual "T1" (encode x) (myencode x))
       , let x = (Bar [Foo 1 'x'] "hello") in TestCase (assertEqual "T2" (encode x) (myencode x))
       , let x = (Fork 'a' [Fork 'b' []]) in TestCase (assertEqual "T3" (encode x) (myencode x))
       , let x = (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))) in TestCase (assertEqual "T4" (encode x) (myencode x))
       , let x = (WithInt 5 (WithInt 2 'a')) in TestCase (assertEqual "T5" (encode x) (myencode x))
       , let loc = $(lift =<< location) in TestCase (assertEqual "T6" (encode loc) (myencode loc))
       , let x = ([1,2,3] :: [Int]) in TestCase (assertEqual "T7" (encode x) (myencode x))
       , let x = ([1] :: [Int]) in TestCase (assertEqual "T8" (encode x) (myencode x))
       , let x = (1 :: Int) in TestCase (assertEqual "T9" (encode x) (myencode x))
       , let x = ('x',(2 :: Int),("abc" :: String)) in TestCase (assertEqual "T10" (encode x) (myencode x))
       ])
  case (errors, failures) of
    (0, 0) -> return ()
    _ -> error (show r)
#endif
