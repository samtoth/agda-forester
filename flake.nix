{
  description = "Agda backend for Forester";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    agda = {
      url = "github:agda/agda";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
    };

    nixpkgs-tl.url = "github:NixOS/nixpkgs?rev=b3d51a0365f6695e7dd5cdf3e180604530ed33b4";
    treelist = {
      url = "github:samtoth/treelist";
      inputs.nixpkgs.follows = "nixpkgs-tl";
    };

    forester.url = "sourcehut:~jonsterling/ocaml-forester";
  };

  outputs = inputs@{ agda , flake-parts , forester , treelist , ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem = {pkgs , system , lib , ...}: let
        agda-forester = pkgs.haskell.packages.ghc910.developPackage {
          root = lib.fileset.toSource {
            root = ./. ;
            fileset = lib.fileset.unions [
              ./src
              ./app
              ./agda-forester.cabal
              ./LICENSE
            ];
          };
          modifier = pkgs.haskell.lib.compose.overrideCabal (drv: {
            enableLibraryProfiling = false;
            enableExecutableProfiling = false;
            doHaddock = false;
          });
        };

      in {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [ inputs.agda.overlays.default ];
        };

        packages.default = agda-forester;
        devShells.default = pkgs.mkShell {
          packages = [
            forester.packages.${system}.default
            treelist.packages.${system}.default
          ];
        };
      };

      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
    };
}
