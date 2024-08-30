/* Based on: https://github.com/srid/haskell-template */

{
  description = "Mulky Backups via Bupstash";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ ];

        pkgs =
          import nixpkgs { inherit system overlays; config.allowBroken = true; };

        project = returnShellEnv:
          pkgs.haskellPackages.developPackage {
            inherit returnShellEnv;

            name = "mulkup";

            root = pkgs.nix-gitignore.gitignoreSource [ ] ./.;

            withHoogle = false;

            overrides = self: super: with pkgs.haskell.lib; {
              # Use callCabal2nix to override Haskell dependencies here
              # cf. https://tek.brick.do/K3VXJd8mEKO7
            };

            modifier = drv:
              pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
              [
                cabal-fmt
                cabal-install
                ghcid
                haskell-language-server
                ormolu
              ]);
          };

      in
      {
        defaultPackage = project false;
        devShell = project true;
      });
}
