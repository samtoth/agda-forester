pkgs: super:
let
  forester = builtins.getFlake "sourcehut:~jonsterling/ocaml-forester?rev=56de06afe952d752c1a13fdcd8bb56c5fef9956f";
in {
    forester = forester.packages.${builtins.currentSystem}.default;
   }