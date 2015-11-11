{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, aeson-pretty, async, base, bytestring
      , cond, directory, exceptions, hastache, hspec, http-client, lens
      , lens-aeson, mtl, random, regex-compat, shelly, Spock, stdenv, stm
      , text, time, transformers, unix, unordered-containers, uuid, wreq
      , yesod
      }:
      mkDerivation {
        pname = "shelduck";
        version = "0.1.4.2";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson aeson-pretty async base bytestring cond directory exceptions
          hastache http-client lens lens-aeson mtl random shelly Spock stm
          text time transformers unordered-containers uuid wreq yesod
        ];
        executableHaskellDepends = [
          aeson aeson-pretty async base bytestring cond directory exceptions
          hastache http-client lens lens-aeson mtl random shelly Spock stm
          text time transformers unix unordered-containers uuid wreq yesod
        ];
        testHaskellDepends = [
          aeson base hspec http-client lens mtl regex-compat stm text
          transformers wreq
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
