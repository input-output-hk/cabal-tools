{ pkgs, ... }: {
  # name = "project-name";
  compiler-nix-name = "ghc926"; # Version of GHC to use
  index-state = "2023-03-09T00:00:00Z";

  # Cross compilation support:
  # crossPlatforms = p: pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 ([
  #   p.mingwW64
  #   p.ghcjs
  # ] ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
  #   p.musl64
  # ]);

  # Tools to include in the development shell
  shell.tools.cabal = "latest";
  # shell.tools.hlint = "latest";
  # shell.tools.haskell-language-server = "latest";
}
