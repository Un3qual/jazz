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
        hsPkgs = pkgs.haskell.packages.ghc948.override {
          overrides = self: super: {
            diagnose = pkgs.haskell.lib.dontCheck
              (pkgs.haskell.lib.doJailbreak
                (pkgs.haskell.lib.markUnbroken super.diagnose));
            qbe = pkgs.haskell.lib.dontCheck
              (pkgs.haskell.lib.markUnbroken super.qbe);
          };
        };
        jazzHs = hsPkgs.callCabal2nix "jazz" ./jazz-hs { };
      in {
        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            stack
            ghcForStack
            cabal-install
            ormolu
            hlint
            git
            ripgrep
            nodePackages.prettier
          ];
        };

        # Use Nix-native package checks to avoid Stack's network-dependent
        # snapshot/package fetch path inside sandboxed flake checks.
        checks.jazz-test-suite = pkgs.haskell.lib.overrideCabal jazzHs (_: {
          doCheck = true;
        });
      });
}
