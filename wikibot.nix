{ mkDerivation, aeson, base, bloodhound, bytestring, containers
, cryptonite, errors, filepath, hspec, http-client, memory, pandoc
, pandoc-types, shake, slack-api, stdenv, text, yaml
}:
mkDerivation {
  pname = "wikibot";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bloodhound bytestring containers cryptonite errors
    filepath http-client pandoc pandoc-types shake slack-api text yaml
  ];
  testHaskellDepends = [
    aeson base bloodhound bytestring containers cryptonite errors
    filepath hspec http-client memory shake text yaml
  ];
  description = "Use a slack bot to search a github wiki";
  license = stdenv.lib.licenses.gpl3;
}
