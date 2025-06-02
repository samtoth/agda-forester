{ # Is this a nix-shell invocation?
  inNixShell ? false
  # Do we want the full Agda package for interactive use? Set to false in CI
, interactive ? true
, system ? builtins.currentSystem
}:
let
  pkgs = import ./nix/nixpkgs.nix { inherit system; };
  forester = builtins.getFlake "sourcehut:~jonsterling/ocaml-forester?rev=56de06afe952d752c1a13fdcd8bb56c5fef9956f";

  myForester = forester.legacyPackages.${system};

  overlay = final: prev: {
    ocamlfind =
      prev.ocamlfind.overrideAttrs (_: {
        version = "1.9.6";
        src = pkgs.fetchurl {
          url = "http://download2.camlcity.org/download/findlib-1.9.6.tar.gz";
          sha256 = "sha256-LfmWJ5rha2Bttf9Yefk9v63giY258aPoL3+EX6opMKI=";
        };
      });
          };
  myForester' = myForester.overrideScope' overlay;

  hsPkgs = pkgs.ourHaskellPackages;
  agdaForester = (hsPkgs.callCabal2nix "agda-forester" ./. {}).overrideAttrs(old: {
            buildInputs = (old.buildInputs or []) ++ [
              myForester'.forester
            ];

            propagatedBuildInputs = (old.propagatedBuildInputs or [])
                                    ++ [ myForester'.forester ];
        }
  );
in agdaForester
