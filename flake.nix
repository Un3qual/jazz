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
        jazzNext = pkgs.haskell.lib.enableCabalFlag
          (hsPkgs.callCabal2nix "jazz-next" ./jazz-next { })
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
            toolPkgs.nodePackages.prettier
          ];
        };

        checks.jazz-next-test-suite = pkgs.haskell.lib.overrideCabal jazzNext (_: {
          doCheck = true;
        });
      });
}
