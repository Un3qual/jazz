{
  description = "Jazz development and spec-cleanup checks";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nixpkgs-tools.url = "github:NixOS/nixpkgs/ac62194c3917d5f474c1a844b6fd6da2db95077d";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, nixpkgs-tools, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        toolPkgs = import nixpkgs-tools { inherit system; };
        ghc = pkgs.haskell.compiler.ghc9141;
        hsPkgs = pkgs.haskell.packages.ghc9141;
        jazzSource = pkgs.lib.fileset.toSource {
          root = ./.;
          fileset = pkgs.lib.fileset.unions [
            ./.gitignore
            ./AGENTS.md
            ./LICENSE
            ./PERFORMANCE.md
            ./app
            ./benchmark
            ./cabal.project
            ./cabal.project.profile-hotspots
            ./cabal.project.profile-stages
            ./docs/compiler/architecture.md
            ./editors/vscode-jazz
            ./flake.nix
            ./jazz
            ./jazz.cabal
            ./program-support
            ./programs
            ./scripts
            ./src
            ./test
          ];
        };
        jazz = pkgs.haskell.lib.enableCabalFlag
          (hsPkgs.callCabal2nix "jazz" jazzSource { })
          "development";
      in {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            ghc
            cabal-install
            ormolu
            hlint
            git
            ripgrep
            nodejs_22
            toolPkgs.nodePackages.prettier
          ];
        };

        checks.jazz-test-suite = pkgs.haskell.lib.overrideCabal jazz (_: {
          doCheck = true;
        });
      });
}
