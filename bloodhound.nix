{ mkDerivation, aeson, base, blaze-builder, bytestring, containers
, data-default-class, errors, exceptions, generics-sop, hashable
, hspec, http-client, http-types, mtl, mtl-compat, network-uri
, QuickCheck, quickcheck-properties, scientific, semigroups, stdenv
, temporary, text, time, transformers, unix-compat
, unordered-containers, vector, fetchFromGitHub
}:
mkDerivation {
  pname = "bloodhound";
  version = "0.16.0.0";
  src = fetchFromGitHub {
    owner = "rvl";
    repo = "bloodhound";
    rev = "4335e9a8d1f887f822f2a63851965edf2a293fd8";
    sha256 = "0qq7887rfybhl0gv4x7qvhznm1hlxhfsrvxv0z2h022j4ln7ryh6";
  };
  libraryHaskellDepends = [
    aeson base blaze-builder bytestring containers data-default-class
    exceptions hashable http-client http-types mtl mtl-compat
    network-uri scientific semigroups text time transformers
    unordered-containers vector
  ];
  testHaskellDepends = [
    aeson base bytestring containers errors exceptions generics-sop
    hspec http-client http-types mtl network-uri QuickCheck
    quickcheck-properties semigroups temporary text time unix-compat
    unordered-containers vector
  ];
  homepage = "https://github.com/bitemyapp/bloodhound";
  description = "ElasticSearch client library for Haskell";
  license = stdenv.lib.licenses.bsd3;
}
