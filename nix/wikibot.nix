{ mkDerivation, aeson, async, base, bloodhound, bytestring, colour
, containers, cryptonite, errors, exceptions, filepath, formatting
, http-client, http-types, network-uri, optparse-applicative
, pandoc, pandoc-types, safe, say, scotty, shake, slack-api, split
, stdenv, text, time, transformers, wai, wai-extra, warp, yaml
, src
}:
mkDerivation {
  pname = "wikibot";
  version = "0.1.0.0";
  inherit src;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base bloodhound bytestring colour containers cryptonite
    errors exceptions filepath formatting http-client http-types
    network-uri optparse-applicative pandoc pandoc-types safe say
    scotty shake slack-api split text time transformers wai wai-extra
    warp yaml
  ];
  description = "Use a slack bot to search a github wiki";
  license = stdenv.lib.licenses.gpl3;
}
