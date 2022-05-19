{
  description = "tsearch";

  inputs = {
    nixpkgs.url = "nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system: let
      pkgs = import nixpkgs {
        inherit system;
        config.allowBroken = true;
      };
      ghc = import ./nix/override-ghc.nix pkgs.haskell.packages.ghc922;
      server = ghc.callCabal2nix "server" ./server/. {};
    in {
      devShell = import ./shell.nix {
        inherit pkgs server;
      };
      defaultPackage = server;
      packages = flake-utils.lib.flattenTree {
        inherit server;
      };
    });
}
