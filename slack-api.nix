{ mkDerivation, aeson, base, bytestring, containers, errors
, fetchgit, hashable, io-streams, lens, lens-aeson, monad-loops
, mtl, network, network-uri, scientific, stdenv, text, time
, time-locale-compat, tls, transformers, websockets, wreq, wuss
}:
mkDerivation {
  pname = "slack-api";
  version = "0.12";
  src = fetchgit {
    url = "https://github.com/mpickering/slack-api.git";
    sha256 = "16gzqsk5g0d3d84x8d992b5xpsy6dj68k40nlbcdar7i2vfzrrcd";
    rev = "02553f682117dd2422ab9418ff5df9914d7047ea";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers errors hashable io-streams lens
    lens-aeson monad-loops mtl network network-uri scientific text time
    time-locale-compat tls transformers websockets wreq wuss
  ];
  testHaskellDepends = [ base ];
  description = "Bindings to the Slack RTM API";
  license = stdenv.lib.licenses.mit;
}
