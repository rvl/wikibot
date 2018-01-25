{ mkDerivation, aeson, base, bloodhound, bytestring, cryptonite
, errors, hspec, http-client, shake, slack-api, stdenv, text, yaml
}:
mkDerivation {
  pname = "wikibot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bloodhound bytestring cryptonite errors http-client
    shake slack-api text yaml
  ];
  testHaskellDepends = [
    aeson base bloodhound bytestring errors hspec http-client shake
    text yaml
  ];
  description = "Use a slack bot to search a github wiki";
  license = stdenv.lib.licenses.gpl3;
}
