{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages' = if compiler == "default"
                        then pkgs.haskellPackages
                        else pkgs.haskell.packages.${compiler};

  haskellPackages = haskellPackages'.override {
    overrides =
      self: super: {
        # Problem 1: no hackage release
        # https://github.com/mpickering/slack-api/issues/84
        # Problem 2: no rich message buttons => use jkarni fork
        # https://github.com/jkarni/slack-api
        # Problem 3: missing events API => use my fork
        # https://github.com/rvl/slack-api
        slack-api = pkgs.haskell.lib.dontCheck (self.callPackage ./slack-api.nix {});

        # add missing char filters to mappings api
        bloodhound = pkgs.haskell.lib.dontCheck (self.callPackage ./bloodhound.nix {});
      };
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage ./nix/wikibot.nix {
    src = pkgs.lib.sourceFilesBySuffices ./. [".hs" ".cabal" "LICENSE"];
  });

in

  if pkgs.lib.inNixShell then drv.env else drv
