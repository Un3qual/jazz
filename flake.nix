{
  description = "Jazz development and spec-cleanup checks";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
        ghcForStack = pkgs.haskell.compiler.ghc948;
      in {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            stack
            ghcForStack
            cabal-install
            ormolu
            hlint
            git
          ];
        };

        checks.parser-tests = pkgs.runCommand "parser-tests" {
          nativeBuildInputs = [
            pkgs.stack
            ghcForStack
            pkgs.cabal-install
            pkgs.git
            pkgs.stdenv.cc
            pkgs.gnumake
            pkgs.pkg-config
            pkgs.libffi
            pkgs.zlib
          ];
        } ''
          export HOME="$(mktemp -d)"
          export STACK_ROOT="$HOME/.stack"

          cp -R ${self}/jazz-hs ./jazz-hs
          chmod -R +w ./jazz-hs
          cd ./jazz-hs

          # Parse-focused verification check required by the plan.
          # The suite includes parser groups via `ParserSpec`.
          stack --system-ghc --no-install-ghc test

          mkdir -p "$out"
        '';
      });
}
