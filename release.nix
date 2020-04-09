let
  config = {
    allowBroken = true;
    packageOverrides = pkgs: with pkgs.haskell.lib; rec {
        haskellPackages = pkgs.haskellPackages.override {
            overrides = self: super: {
                persistent-postgresql = dontCheck super.persistent-postgresql;
                restclient-api = self.callPackage ./default.nix {};
            };
        };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
in pkgs.haskellPackages.restclient-api
