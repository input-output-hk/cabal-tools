{
  # This is a template created by `hix init`
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    let
      supportedSystems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];
    in
    flake-utils.lib.eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          inherit (haskellNix) config;
          overlays = [ haskellNix.overlay ];
        };
        project = pkgs.haskell-nix.cabalProject' {
          src = ./.;

          name = "cabal-tools";
          compiler-nix-name = "ghc927";
          configureArgs = "-v3";

          # We absolutely need cabal 3.10.1.0
          cabal-install = pkgs.cabal-install; # version 3.10.1.0

          crossPlatforms = p:
            pkgs.lib.optionals (pkgs.stdenv.system == "x86_64-linux")
              [ p.mingwW64 p.musl64 ];

          shell.tools.cabal = "latest";
          shell.tools.hlint = "latest";
          shell.tools.haskell-language-server = "latest";
        };
        flake = project.flake {
          variants = {
            "3.8.1.0" = { cabalProjectLocal = "constraints: cabal-install ==3.8.1.0"; };
            "3.10.1.0" = { cabalProjectLocal = "constraints: cabal-install ==3.10.1.0"; };
          };
        };
      in
      flake // {
        inherit project;
      });

  # --- Flake Local Nix Configuration ----------------------------
  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    allow-import-from-derivation = "true";
  };
}
