{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, async, base, bytestring, hastache
      , hspec, lens, lens-aeson, rainbow, random, regex-compat, shelly
      , Spock, stdenv, stm, text, transformers, wreq
      }:
      mkDerivation {
        pname = "scalpel";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson async base bytestring hastache lens lens-aeson rainbow random
          shelly Spock stm text transformers wreq
        ];
        testHaskellDepends = [
          aeson base hspec lens regex-compat stm text transformers wreq
        ];
        license = stdenv.lib.licenses.asl20;
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
