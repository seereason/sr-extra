{-# LANGUAGE ConstraintKinds, DataKinds, DeriveAnyClass, DeriveGeneric, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables, TypeApplications, TypeFamilies #-}

-- | Type level "sets" as a cons list constructed of Eithers.

module Extra.ErrorSet
  ( Insert
  , Delete
  , Member(follow)
  , OneOf(OneOf, unOneOf)
  -- * Re-export
  -- , Void
  ) where

import Control.Lens
import Data.SafeCopy
import Data.Serialize
import Data.Typeable (Typeable)
import GHC.Generics

-- See SafeCopy issue #78
-- instance SafeCopy Void

type family Insert t s where
  Insert t (Either t a) = Either t a
  Insert t () = Either t ()
  Insert t (Either a b) = Either a (Insert t b)

type family Delete t s where
  Delete t (Either t a) = a
  Delete t () = ()
  Delete t (Either a b) = Either a (Delete t b)

class Member t set where follow :: Prism' set t
instance Member t (Either t a) where follow = _Left
instance {-# OVERLAPS #-} Member t b => Member t (Either a b) where follow = _Right . follow
instance Member t () where follow = error "Type Set Error"
instance Member t (OneOf set) where follow = iso OneOf unOneOf . follow

newtype OneOf (set :: *) = OneOf {unOneOf :: set} deriving (Show, Eq, Ord, Generic, Serialize, Typeable)
instance (SafeCopy set, Typeable set) => SafeCopy (OneOf set)
