args:
# We have to temporarily override ocamlPackages - Maybe findlib mirror will come back up
# or if not, will give a better solution to this.
let ocamlOverlay = pkgs: super: {
    ocamlPackages = pkgs.ocamlPackages.overrideScope' (oself: osuper: {
    findlib = osuper.findlib.overrideAttrs (old: {
      version = "1.9.6";
      src = pkgs.fetchurl {
        url = "http://download2.camlcity.org/download/findlib-1.9.6.tar.gz";
        sha256 = "0ci6nps2qgkhfjqji18qjc26rid9gkpmxzlb1svg5wwair0qvb0s";
      };
    });
    });
    };
in
import (builtins.fetchTarball {
  name   = "nixpkgs";
  url    = "https://github.com/nixos/nixpkgs/archive/11cb3517b3af6af300dd6c055aeda73c9bf52c48.tar.gz";
  sha256 = "sha256:1915r28xc4znrh2vf4rrjnxldw2imysz819gzhk9qlrkqanmfsxd";
}) ({
  overlays = [
    (import ./haskell-packages.nix)
    (ocamlOverlay)
    ];
} // args)
