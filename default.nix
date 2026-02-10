{ # Is this a nix-shell invocation?
  inNixShell ? false
  # Do we want the full Agda package for interactive use? Set to false in CI
, interactive ? true
, system ? builtins.currentSystem
}:
let
  pkgs = import ./nix/nixpkgs.nix { inherit system; };
  forester = builtins.getFlake "sourcehut:~jonsterling/ocaml-forester?rev=58af4f74e009655999302e63c9c58f9640c262ad";

  myForester = forester.legacyPackages.${system};

  overlay = final: prev: {
    dune = prev.dune.overrideAttrs (o: {
      version = "3.17.2";
      src = builtins.fetchurl {
        url = "https://github.com/ocaml/dune/releases/download/3.17.2/dune-3.17.2.tbz";
        sha256 = "0r7al83jwkdfk6qvb53vrlzzfr08gwcydn1ccigfdsfg1vnzxslx";
      };
      nativeBuildInputs = o.nativeBuildInputs ++ [ pkgs.makeWrapper ];
      postFixup =
        if pkgs.stdenv.isDarwin then ''
           wrapProgram $out/bin/dune \
           --suffix PATH : "${pkgs.darwin.sigtool}/bin"
        '' else "";
    });

    forester = prev.forester.overrideAttrs (_: {
      DUNE_CACHE = "enabled";
      DUNE_CACHE_TRANSPORT = "direct";
      XDG_CACHE_HOME = "/tmp/dune-cache";
    });
  };
  myForester' = myForester.overrideScope overlay;

  treelist = builtins.getFlake "github:samtoth/treelist";

  hsPkgs = pkgs.ourHaskellPackages;
  agdaForester = (hsPkgs.callCabal2nix "agda-forester" ./. {}).overrideAttrs(old: {
            buildInputs = (old.buildInputs or []) ++ [
              myForester'.forester
              treelist.legacyPackages.${system}.treelist
            ];

            nativeBuildInputs = (old.nativeBuildInputs or []) ++ [
              treelist.legacyPackages.${system}.treelist
            ];

            propagatedBuildInputs = (old.propagatedBuildInputs or [])
                                    ++ [ myForester'.forester
                                         treelist.legacyPackages.${system}.treelist
                                       ];
        }
  );
in agdaForester
