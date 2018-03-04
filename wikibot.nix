{ mkDerivation, aeson, async, base, bloodhound, bytestring, colour
, containers, cryptonite, errors, filepath, formatting, hspec
, http-client, http-types, memory, network-uri, pandoc
, pandoc-types, say, scotty, shake, slack-api, split, stdenv, text
, wai, wai-extra, warp, yaml
}:
mkDerivation {
  pname = "wikibot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base bloodhound bytestring colour containers cryptonite
    errors filepath formatting http-client http-types network-uri
    pandoc pandoc-types say scotty shake slack-api split text wai
    wai-extra warp yaml
  ];
  testHaskellDepends = [
    aeson base bloodhound bytestring containers cryptonite errors
    filepath hspec http-client memory shake text yaml
  ];
  description = "Use a slack bot to search a github wiki";
  license = stdenv.lib.licenses.gpl3;
}
