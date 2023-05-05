{ pkgs, ... }: {
  name = "cabal-tools";
  compiler-nix-name = "ghc927"; # Version of GHC to use

  # Cross compilation support:
  crossPlatforms = p:
    pkgs.lib.optionals (pkgs.stdenv.system == "x86_64-linux")
      [ p.mingwW64 p.musl64 ];

  # Tools to include in the development shell
  shell.tools.cabal = "latest";
  shell.tools.hlint = "latest";
  shell.tools.haskell-language-server = "latest";
}
