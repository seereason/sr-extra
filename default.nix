{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, bzlib, Cabal, cereal
      , containers, Diff, directory, exceptions, fgl, filemanip, filepath
      , generic-data, hslogger, HUnit, lens, ListLike, mmorph, mtl
      , network-uri, pretty, process, process-extras, pureMD5, QuickCheck
      , random, safecopy, show-combinators, show-please, stdenv, syb
      , template-haskell, text, th-lift, th-lift-instances, th-orphans
      , time, transformers, unexceptionalio-trans, unix, Unixutils
      , userid, uuid, uuid-orphans, uuid-types, zlib
      }:
      mkDerivation {
        pname = "sr-extra";
        version = "1.65";
        src = ./.;
        libraryHaskellDepends = [
          base bytestring bzlib Cabal cereal containers Diff directory
          exceptions fgl filemanip filepath generic-data hslogger HUnit lens
          ListLike mmorph mtl network-uri pretty process process-extras
          pureMD5 QuickCheck random safecopy show-combinators show-please syb
          template-haskell text th-lift th-lift-instances th-orphans time
          transformers unexceptionalio-trans unix Unixutils userid uuid
          uuid-orphans uuid-types zlib
        ];
        homepage = "https://github.com/seereason/sr-extra";
        description = "Module limbo";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
