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
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        overlays = [ ];

        pkgs =
          import nixpkgs { inherit system overlays; config.allowBroken = true; };

        co-log-src = pkgs.fetchgit
          {
            url = "https://github.com/kowainik/co-log";
            sha256 = "sha256-XV1xh3aBRY2XwWjGRkd7F2DHf3zTeeFawHJzf05eaWE"; # pkgs.lib.fakeSha256;
            rev = "72fbe394b437c698d574cd7604ad3f7f807383e0";
            fetchSubmodules = false;
          };

        project = returnShellEnv:
          pkgs.haskellPackages.developPackage {
            inherit returnShellEnv;

            name = "mulkup";

            root = pkgs.nix-gitignore.gitignoreSource [ ] ./.;

            withHoogle = false;

            overrides = self: super: with pkgs.haskell.lib; {
              # Use callCabal2nix to override Haskell dependencies here
              # cf. https://tek.brick.do/K3VXJd8mEKO7

              co-log-polysemy = pkgs.haskell.lib.dontCheck
                (self.callCabal2nixWithOptions "co-log-polysemy" co-log-src "--subpath=co-log-polysemy" { });

              doctest = pkgs.haskell.lib.dontCheck
                (self.callHackage "doctest" "0.16.3" { });

              optics = pkgs.haskell.lib.dontCheck
                (self.callHackage "optics" "0.4" { });
              optics-core = pkgs.haskell.lib.dontCheck
                (self.callHackage "optics-core" "0.4" { });
              optics-th = pkgs.haskell.lib.dontCheck
                (self.callHackage "optics-th" "0.4" { });
              optics-extra = pkgs.haskell.lib.dontCheck
                (self.callHackage "optics-extra" "0.4" { });
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
