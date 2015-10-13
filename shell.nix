{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, async, base, bytestring, hastache
      , hspec, lens, lens-aeson, random, regex-compat, shelly, Spock
      , stdenv, stm, text, time, transformers, uuid, wreq
      }:
      mkDerivation {
        pname = "shelduck";
        version = "0.1.3.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson async base bytestring hastache lens lens-aeson random shelly
          Spock stm text time transformers uuid wreq
        ];
        executableHaskellDepends = [
          aeson async base bytestring hastache lens lens-aeson random shelly
          Spock stm text time transformers uuid wreq
        ];
        testHaskellDepends = [
          aeson base hspec lens regex-compat stm text transformers wreq
        ];
        description = "Test webhooks locally";
        license = stdenv.lib.licenses.asl20;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
