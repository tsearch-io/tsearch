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
      server = import ./server/. pkgs;
    in {
      devShell = import ./shell.nix {
        inherit pkgs;
      };
      defaultPackage = server;
      packages = flake-utils.lib.flattenTree {
        inherit server;
      };
    });
}
