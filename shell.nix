{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, async, base, lens, lens-aeson, rainbow
      , Spock, stdenv, stm, text, wreq
      }:
      mkDerivation {
        pname = "scalpel";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          aeson async base lens lens-aeson rainbow Spock stm text wreq
        ];
        license = stdenv.lib.licenses.asl20;
      };

  haskellPackages = if compiler == "default"
                      then pkgs.haskellPackages
                      else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
