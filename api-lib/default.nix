{ pkgs, ... }:
  pkgs.haskell.packages.ghc922.callCabal2nix "tsearch-api-lib" ./. {}
