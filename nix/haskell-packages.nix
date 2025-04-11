pkgs: super:
let
  thunkSource = (import ./nix-thunk { inherit pkgs; }).thunkSource;
  noJunk = x: pkgs.haskell.lib.overrideCabal x {
    doCheck = false;
    doHaddock = false;
    testHaskellDepends = [];
  };
  noProfile = x: pkgs.haskell.lib.overrideCabal x {
    enableExecutableProfiling = false;
    enableLibraryProfiling = false;
  };
in
  {
    # Can't just override all Haskell packages because callCabal2nix
    # somehow depends on mime-types
    ourHaskellPackages = super.haskell.packages.ghc9101.override (old: {
      overrides = self: super: {
        Agda = noJunk (super.callCabal2nixWithOptions "Agda" (thunkSource ./Agda) "-f optimise-heavily -f debug" {});
      };
    });
  }