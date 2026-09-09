{
  description = "Jazz development and spec-cleanup checks";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    nixpkgs-tools.url = "github:NixOS/nixpkgs/ac62194c3917d5f474c1a844b6fd6da2db95077d";
    weeder-src = {
      url = "github:ocharles/weeder/9f2280826b0a9a7114198e262e95c568e595b35c";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, nixpkgs-tools, weeder-src, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        toolPkgs = import nixpkgs-tools { inherit system; };
        ghc = pkgs.haskell.compiler.ghc9141;
        hsPkgs = pkgs.haskell.packages.ghc9141;
        qualityHsPkgs = hsPkgs.override {
          overrides = final: previous: {
            algebraic-graphs = final.algebraic-graphs_0_8;
            # toml-reader's test-only barbies dependency fails its upstream
            # suite on GHC 9.14. Keep Weeder's own integration tests enabled.
            toml-reader = pkgs.haskell.lib.dontCheck previous.toml-reader;
            # GHC 9.14 changes rejection messages and optimized Core shape.
            # Keep semantic tests; omit error-text doctests and Core inspection.
            generic-lens = pkgs.haskell.lib.overrideCabal previous.generic-lens (_: {
              testTargets = [ "generic-lens-syb-tree" "generic-lens-bifunctor" ];
            });
          };
        };
        weeder = pkgs.haskell.lib.overrideCabal
          (qualityHsPkgs.callCabal2nix "weeder" weeder-src { })
          (previous: {
            # GHC 9.14 records the evidence used by these syntax forms. Remove
            # only obsolete expected-failure markers: all 22 tests still run,
            # now checking the upstream correct (empty) .stdout expectations.
            postPatch = (previous.postPatch or "") + ''
              rm test/Spec/OverloadedLists.failing \
                 test/Spec/OverloadedStrings.failing \
                 test/Spec/RangeEnum.failing \
                 test/Spec/ApplicativeDo.failing \
                 test/Spec/NumInstanceLiteral.failing
            '';
          });
        jazzSource = pkgs.lib.fileset.toSource {
          root = ./.;
          fileset = pkgs.lib.fileset.unions [
            ./.gitignore
            ./.hlint.yaml
            ./weeder-production.toml
            ./weeder.toml
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
          inherit jazz weeder;
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

        devShells.quality = pkgs.mkShell {
          inputsFrom = [ self.devShells.${system}.default ];
          # Do not propagate Weeder's bare GHC ahead of Jazz's package wrapper.
          packages = [ (pkgs.writeShellScriptBin "weeder" ''
            exec ${weeder}/bin/weeder "$@"
          '') ];
        };

        devShells.docs = pkgs.mkShell {
          packages = documentationTools;
        };

        checks.jazz-test-suite = jazz;
      });
}
