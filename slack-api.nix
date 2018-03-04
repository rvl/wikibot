{ mkDerivation, aeson, base, bytestring, containers, errors
, fetchgit, hashable, io-streams, lens, lens-aeson, monad-loops
, mtl, network, network-uri, scientific, stdenv, text, time
, time-locale-compat, tls, transformers, websockets, wreq, wuss
}:
mkDerivation {
  pname = "slack-api";
  version = "0.12";
  # src = fetchgit {
  #   url = "https://github.com/mpickering/slack-api.git";
  #   sha256 = "16gzqsk5g0d3d84x8d992b5xpsy6dj68k40nlbcdar7i2vfzrrcd";
  #   rev = "02553f682117dd2422ab9418ff5df9914d7047ea";
  # };
  src = fetchgit {
    url = "https://github.com/jkarni/slack-api.git";
    rev = "58e13fa6df986a27f90bdacf1f3e25823e77eee3";
    sha256 = "0vjqqagpl6lfr6gjhh1r77462j0fh375g7832cgy2yy8pycgdmpp";
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
