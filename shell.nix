{
  pkgs,
  server,
  ...
}:
pkgs.mkShell {
  inputsFrom = [
    server.env
  ];
  buildInputs = with pkgs; [
    haskell-language-server
    haskellPackages.cabal-install
    haskellPackages.ghcid
    haskellPackages.hlint
    haskellPackages.hpack
    haskellPackages.retrie
    haskellPackages.fourmolu
    haskellPackages.hoogle
    niv

    glibcLocales
  ];
  withHoogle = true;
  LANG = "en_US.utf8";
}
