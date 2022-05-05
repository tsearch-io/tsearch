{pkgs, ...}: let
  myHaskell = pkgs.callPackage ../nix/haskell.nix {};
in
  myHaskell.callCabal2nix "server" ../server/. {}
