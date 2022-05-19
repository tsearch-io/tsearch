ghc: ghc.override {
    overrides = final: prev: rec {
      tsearch-api-lib = final.callCabal2nix "tsearch-api-lib" ../api-lib/. {};
    };
  }
