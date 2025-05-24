pkgs: super:
let
  forester = builtins.getFlake "sourcehut:~jonsterling/ocaml-forester?rev=56de06afe952d752c1a13fdcd8bb56c5fef9956f";
in {
  ocamlPackages = super.ocamlPackages // {
    findlib = super.ocamlPackages.findlib.overrideAttrs (old : {
      src = super.fetchurl {
        url = "http://download2.camlcity.org/download/findlib${old.version}.tar.gz";
        sha256 = "0ci6nps2qgkhfjqji18qjc26rid9gkpmxzlb1svg5wwair0qvb0s";
      };
    });
  };
  forester = forester.packages.${builtins.currentSystem}.default;
}
