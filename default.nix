{ # Is this a nix-shell invocation?
  inNixShell ? false
  # Do we want the full Agda package for interactive use? Set to false in CI
, interactive ? true
, system ? builtins.currentSystem
}:
let
  pkgs = import ./nix/nixpkgs.nix { inherit system; };
  forester = builtins.getFlake "sourcehut:~jonsterling/ocaml-forester?rev=56de06afe952d752c1a13fdcd8bb56c5fef9956f";

  myForester = forester.packages.${system}.default //
               {
                 overrideAttributes = old: {
                   ocamlPackages = old.ocamlPackages.findlib.overrideAttrs (old': {
                     src = pkgs.fetchurl {
                       url = "http://download2.camlcity.org/download/findlib${old'.version}.tar.gz";
                       sha256 = "0ci6nps2qgkhfjqji18qjc26rid9gkpmxzlb1svg5wwair0qvb0s";
                     };
                   });
                 };
               };

  hsPkgs = pkgs.ourHaskellPackages;
  agdaForester = hsPkgs.callCabal2nix "agda-forester" ./. {} //
    {
        overrideAttributes = old: {
            buildInputs = (old.buildInputs or []) ++ [
                # forester.packages.${builtins.currentSystem}.default;
              myForester
            ];
        };
    };
in agdaForester // {passthru = agdaForester.passthru // {forest = myForester;};}
