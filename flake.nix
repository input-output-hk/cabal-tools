{
  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-nix.url = "github:input-output-hk/haskell.nix";
  };
  outputs = inputs@{ flake-parts, haskell-nix, ... }:
    flake-parts.lib.mkFlake
      { inherit inputs; }
      {
        systems = [ "x86_64-linux" ];
        perSystem = { self', system, pkgs, ... }:
          let
            project = pkgs.haskell-nix.cabalProject' {
              name = "cabal-tools";
              compiler-nix-name = "ghc96";

              src = ./.;

              crossPlatforms = p:
                pkgs.lib.optionals
                  (pkgs.stdenv.system == "x86_64-linux")
                  [ p.mingwW64 p.musl64 ];

              shell.tools = {
                cabal = "latest";
                cabal-plan = "latest";
                fourmolu = "0.14.0.0";
              };
            };
            flake = project.flake { };
          in
          {
            _module.args.pkgs = haskell-nix.legacyPackages.${system};
            inherit (flake) apps checks devShells packages;
          };
      };

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
