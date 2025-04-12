{
system ? builtins.currentSystem
}:
let
  pkgs = import ./nix/nixpkgs.nix { inherit system; };
  drv = import ./default.nix { inherit system; };
in pkgs.mkShell {
    name = "agda-forester-shell";

    buildInputs = [
        drv
        pkgs.forester
    ];

    shellHook = ''
    echo "Welcome to the dev shell!"
    echo "Agda version: $(agda --version)"
    echo "Forester version: $(forester --version || echo not found)"
    '';
}