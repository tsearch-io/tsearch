{pkgs, ...}:
  pkgs.haskell.packages.ghc922.override {
    overrides = final: prev: rec {
      tsearch-api-lib = import ../api-lib/. pkgs;
    };
  }
