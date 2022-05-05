{fetchgit, lib, pkgs, ...}:

let
  tie =
    fetchgit {
      url = "https://github.com/scarf-sh/tie";
      rev = "d84aa4a16d4f76f81644ab3ac9d946591b0e1028";
      sha256 = "sha256-7bd7fSnFVNepVTnJR9UCBAxBMWdcYq+kInZCGYo95JQ=";
    };
  myHaskell = pkgs.callPackage ./haskell.nix {};
in
  with pkgs.haskell.lib; justStaticExecutables (
    myHaskell.callCabal2nix "tie" tie {}
  )
