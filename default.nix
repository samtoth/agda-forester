{ # Is this a nix-shell invocation?
  inNixShell ? false
  # Do we want the full Agda package for interactive use? Set to false in CI
, interactive ? true
, system ? builtins.currentSystem
}:
let
  pkgs = import ./nix/nixpkgs.nix { inherit system; };
  hsPkgs = pkgs.ourHaskellPackages;
  agdaForester = hsPkgs.callCabal2nix "agda-forester" ./. {} //
    {
        overrideAttributes = old: {
            buildInputs = (old.buildInputs or []) ++ [
                pkgs.forester
            ];
        };
    };
in agdaForester