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
    rev = "bfd3c4efe62b21c28fe3d244ad36e9b7d5562662";
    sha256 = "0iy0nxwj7zdy7qp5k0dxf85n87ljm8wkqaq9070c9x78b2g6q56w";
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
