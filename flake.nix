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
            ./CHANGELOG.md
            ./CONTRIBUTING.md
            ./LICENSE
            ./PERFORMANCE.md
            ./README.md
            ./RELEASING.md
            ./SECURITY.md
            ./app
            ./benchmark
            ./cabal.project
            ./cabal.project.profile-hotspots
            ./cabal.project.profile-stages
            ./docs
            ./editors/vscode-jazz
            ./examples
            ./flake.lock
            ./flake.nix
            ./jazz
            ./jazz.cabal
            ./program-support
            ./programs
            ./rfcs
            ./scripts
            ./src
            ./test
          ];
        };
        jazzBase = pkgs.haskell.lib.enableCabalFlag
          (hsPkgs.callCabal2nix "jazz" jazzSource { })
          "development";
        jazz = pkgs.haskell.lib.overrideCabal jazzBase (previous: {
          doCheck = true;
          testToolDepends = (previous.testToolDepends or [ ]) ++ [
            pkgs.cabal-install
            pkgs.git
          ];
          preCheck = (previous.preCheck or "") + ''
            export HOME="$TMPDIR/home"
            mkdir -p "$HOME"
          '';
        });
        pinnedPnpm = pkgs.writeShellScriptBin "pnpm" ''
          exec ${pkgs.nodejs_22}/bin/corepack pnpm@11.18.0 "$@"
        '';
        documentationTools = [
          pkgs.git
          pkgs.lychee
          pkgs.python3
          pkgs.ripgrep
          toolPkgs.nodePackages.prettier
        ];
      in {
        packages = {
          inherit jazz;
          default = jazz;
        };

        apps.default = flake-utils.lib.mkApp {
          drv = jazz;
          exePath = "/bin/jazz";
        };

        devShells.default = pkgs.mkShell {
          inputsFrom = [ jazzBase.env ];
          packages = with pkgs; [
            cabal-install
            ormolu
            hlint
            actionlint
            nodejs_22
            pinnedPnpm
          ] ++ documentationTools;
        };

        devShells.docs = pkgs.mkShell {
          packages = documentationTools;
        };

        checks.jazz-test-suite = jazz;
      });
}
