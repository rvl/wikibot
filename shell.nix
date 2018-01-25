{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages' = if compiler == "default"
                        then pkgs.haskellPackages
                        else pkgs.haskell.packages.${compiler};

  haskellPackages = haskellPackages'.override {
    overrides =
      self: super: {
        # https://github.com/mpickering/slack-api/issues/84
        slack-api = pkgs.haskell.lib.dontCheck (self.callPackage ./slack-api.nix {});
      };
  };

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage ./wikibot.nix {});

in

  if pkgs.lib.inNixShell then drv.env else drv
