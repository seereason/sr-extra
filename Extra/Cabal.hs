{-# LANGUAGE OverloadedStrings, ExtendedDefaultRules #-}

module Extra.Cabal where


import Control.Monad (void)
import Data.Text (Text)
import Shelly.Lifted (Sh)
import qualified Shelly.Lifted as Shelly (run)

default (Text)

cabal :: [Text] -> Sh ()
cabal = void . Shelly.run "cabal"

build :: [Text] -> Sh ()
build args = cabal ("new-build" : args)

clean :: [Text] -> Sh ()
clean args = cabal ("new-clean" : args)

configure :: [Text] -> Sh ()
configure args = cabal ("new-configure" : args)

run :: [Text] -> Sh ()
run args = cabal ("new-run" : args)

