args: import (builtins.fetchTarball {
  name   = "nixpkgs";
  url    = "https://github.com/nixos/nixpkgs/archive/c4265ec26dbd5222134be32a2eb10b09e345653e.tar.gz";
  sha256 = "sha256:082g44zybznp148bgs31davi2p83xxal1025k3410zh27x30daps";
}) ({
  overlays = [
    (import ./haskell-packages.nix)
    (import ./forester.nix)
    ];
} // args)
