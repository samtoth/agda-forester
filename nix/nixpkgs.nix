args: import (builtins.fetchTarball {
  name   = "nixpkgs";
  url    = "https://github.com/nixos/nixpkgs/archive/d19cf9dfc633816a437204555afeb9e722386b76.tar.gz";
  sha256 = "sha256:1wirhlw7cqaypgakyfz9ikv7nxdq3il0fk38cdrdmps2zn1l4ccp";
}) ({
  overlays = [ 
    (import ./haskell-packages.nix)
    (import ./forester.nix) 
    ];
} // args)  