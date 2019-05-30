{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}

import Data.Proxy
import Data.Data (Typeable, typeOf, typeRep)
import GHC.Generics

-- For debugging we want show instances
type Debug a = Show a

-- In the following, f and g are types of kind @(* -> *)@ which take
-- type @a@ and map it to the corresponding @Rep a@.

-- newtype 'M1'  i t f p = 'M1' { 'unM1' :: f p }  -- Metadata
-- type    'D1' = 'M1' 'D'                         -- Datatype metadata
-- data    (':+:') f g p = 'L1' (f p) | 'R1' (g p) -- the constructors of a datatype
-- data    'V1'        p                           -- datatype with no constructors
-- type    'C1' = 'M1' 'C'                         -- Constructor metadata
-- newtype 'K1'    i c p = 'K1' { 'unK1' :: c }    -- a constructor
-- type    'Rec0' = 'K1' 'R'                       -- A "recursive" field - its type is not a type parameter
-- data    'U1'        p = 'U1'                    -- constructor with no fields
-- data    (':*:') f g p = (f p) ':*:' (g p)       -- the fields of a constructor
-- type    'S1' = 'M1' 'S'                         -- Field (Selector) metadata
-- newtype (':.:') f g p = 'Comp1' { 'unComp1' :: f (g p) } -- compose types in a field

-- Handle a single field
class DoSingle (f :: * -> *)                                where doSingle :: f a -> Result1
instance (Debug a, Typeable a)       => DoSingle (K1 R a)   where doSingle (K1 x) = doRec0 x
instance Monad f                     => DoSingle (Rec1 f)   where doSingle (Rec1 r) = doRec1 r
instance                                DoSingle Par1       where doSingle (Par1 r) = doPar1 r
instance                                DoSingle (f :.: g)  where doSingle (Comp1 c) = doCompose c

class                                   DoFields f          where doFields :: f a -> String -> Result2
instance (DoFields f, DoFields g)    => DoFields (f :*: g)  where doFields (x :*: y) cname = doFields x cname <> doFields y cname
instance DoSingle f                  => DoFields (M1 S c f) where doFields (M1 x) cname = doS1 cname (doSingle x)
instance                                DoFields U1         where doFields U1 cname = doU1 cname

class                                   DoNamed f           where doNamed :: f a -> String -> Result2
instance (DoNamed f, DoNamed g)      => DoNamed (f :*: g)   where doNamed (x :*: y) cname = doNamed x cname <> doNamed y cname
instance (Selector c, DoSingle f)    => DoNamed (M1 S c f)  where doNamed x'@(M1 x) cname = doS1Named cname (selName x') (doSingle x)
instance                                DoNamed U1          where doNamed U1 cname = doU1 cname

class                                   DoConstr c f        where doConstr :: f a -> String -> Fixity -> M1 C c f a -> Result2
instance DoFields f => DoConstr ('MetaCons s y 'False) f    where doConstr a name Prefix (M1 x) = doFields x name
                                                                  doConstr a name (Infix _ _) (M1 x) = doFields x ("(" ++ name ++ ")")
instance DoNamed f => DoConstr ('MetaCons s y 'True) f      where doConstr a name Prefix (M1 x) = doNamed a name
                                                                  doConstr a name (Infix _ _) (M1 x) = doNamed a ("(" ++ name ++ ")")

class DoType f                                              where doType :: f a -> Result2
instance DoType f                   => DoType (M1 D d f)    where doType (M1 x) = doType x
instance (DoType f, DoType g)      => DoType (f :+: g)      where doType (L1 x) = doType x
                                                                  doType (R1 x) = doType x
instance (Constructor c, DoConstr c f) => DoType (M1 C c f) where doType x@(M1 a) = doConstr a (conName x) (conFixity x) x
instance                                DoType V1           where doType v = case v of {}

type Result1 = [String]
type Result2 = [[String]]

doRec0 :: Show a => a -> Result1
doRec0 x = ["Rec0", show x]

doRec1 :: Monad f => f a -> Result1
doRec1 r = ["Rec1"{-, show r-}] -- doRecord r

doPar1 :: p -> Result1
doPar1 r = ["Par1" {-, show r-}] -- doParameter?

doU1 cname = [["DoNamed U1", cname]]

-- doCompose c -- e.g. a field like [Foo a], which composes [] with Foo.
-- instance (Generic (f (g a)))         => DoSingle (f :.: g)  where doSingle proxy (Comp1 c) = ["Compose"{-, show c-}] <> doSingle proxy (from c)
doCompose :: forall f g a. f (g a) -> Result1
doCompose c = ["Compose"{-, show c-}]

doS1 :: String -> Result1 -> Result2
doS1 cname r = [["doS1", cname] <> r]

doS1Named :: String -> String -> Result1 -> Result2
doS1Named cname sname r = [["doS1", cname, sname] <> r]

data Foo = Foo Int Char deriving (Generic, Show)
data Bar = Bar [Foo] String deriving (Generic, Show)
data Rose a = Fork a [Rose a] deriving (Generic, Show)
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Generic, Show)
data WithInt a = WithInt Int a deriving (Generic, Show)

main :: IO ()
main = do
  putStrLn (show (doType (from (Foo 1 'x'))))
  putStrLn (show (doType (from (Bar [Foo 1 'x'] "hello"))))
  putStrLn (show (doType (from (Fork 'a'[Fork 'b' []]))))
  putStrLn (show (doType (from (Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))))))
  putStrLn (show (doType (from (WithInt 5 (WithInt 2 'a')))))
