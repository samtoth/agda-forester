args: import (builtins.fetchTarball {
  name   = "nixpkgs";
  url    = "https://github.com/nixos/nixpkgs/archive/11cb3517b3af6af300dd6c055aeda73c9bf52c48.tar.gz";
  sha256 = "sha256:1915r28xc4znrh2vf4rrjnxldw2imysz819gzhk9qlrkqanmfsxd";
}) ({
  overlays = [
    (import ./haskell-packages.nix)
    ];
} // args)
