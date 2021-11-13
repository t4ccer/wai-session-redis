with (import <nixpkgs> {});
mkShell {
  buildInputs = [
    stack
    (pkgs.haskell-language-server.override { supportedGhcVersions = [ "8107" ]; })
    haskell.compiler.ghc8107
    cabal2nix
    redis
  ];
}
