{
  description = "A repository of simple tools for cabal";

  inputs = {
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskell-nix }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs { inherit system; inherit (haskell-nix) config; overlays = [haskell-nix.overlay]; };
      project = pkgs.haskell-nix.cabalProject {
        src = ./.;
        compiler-nix-name = "ghc8107";
        index-state = "2022-09-12T00:00:00Z";
        shell.tools = {
          cabal = {};
          hlint = {};
          haskell-language-server = {};
        };
        shell.buildInputs = with pkgs; [
          nixpkgs-fmt
        ];
      };
    in {
      packages = project.cabal-tools.components.exes;

      devShell = pkgs.mkShell {
        name = "cabal-tools-dev-shell";
      };
    });

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };
}
