{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, errors, hashable, http-types, io-streams, lens, lens-aeson
, monad-loops, mtl, network, network-uri, safe, scientific, scotty
, stdenv, text, time, time-locale-compat, tls, transformers, wai
, wai-extra, websockets, wreq, wuss, fetchFromGitHub
}:
mkDerivation {
  pname = "slack-api";
  version = "0.14";
  # my fork
  src = fetchFromGitHub {
    owner = "rvl";
    repo = "slack-api";
    rev = "73f9ba08ee9e89d67f08a44c97b86f78f9852959";
    sha256 = "034dqxbsbb4k453g7npb6ykkr5xbx0g9kp8kknkahq6fag02zxmy";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring containers errors hashable
    http-types io-streams lens lens-aeson monad-loops mtl network
    network-uri safe scientific scotty text time time-locale-compat tls
    transformers wai wai-extra websockets wreq wuss
  ];
  testHaskellDepends = [ base ];
  description = "Bindings to the Slack RTM API";
  license = stdenv.lib.licenses.mit;
}
