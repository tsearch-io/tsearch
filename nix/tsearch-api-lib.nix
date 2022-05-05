{pkgs, ...}: let
  myHaskell = pkgs.callPackage ./haskell.nix {};
in
  myHaskell.callCabal2nix "tsearch-api-lib" ../api-lib/. {}
