{ mkDerivation, aeson, aeson-pretty, async, base, bytestring, cond
, directory, exceptions, hastache, hspec, http-client, lens
, lens-aeson, mtl, random, regex-compat, shelly, Spock, stdenv, stm
, text, time, transformers, unix, unordered-containers, uuid, wreq
, yesod
}:
mkDerivation {
  pname = "shelduck";
  version = "0.2.0.0";
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
}
