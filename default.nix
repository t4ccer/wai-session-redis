{ pkgs ? import <nixpkgs> {} }:

pkgs.haskell.lib.overrideCabal
  (pkgs.haskell.lib.doCheck (pkgs.haskellPackages.callPackage ./wai-session-redis.nix {}))
  (oldDerivation: {
    buildDepends = [ pkgs.redis ];
    preCheck = ''
      redis-server --daemonize yes
    '';
    postCheck = ''
      redis-cli shutdown
    '';
})
