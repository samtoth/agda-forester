pkgs: super:
let
  thunkSource = (import ./nix-thunk { inherit pkgs; }).thunkSource;
  opamNix = (import ./opam-nix {inherit pkgs; }).lib.${builtins.currentSystem};
  devPackagesQuery = {
      ocaml-base-compiler = "5.1.1";
      ocaml-lsp-server = "*";
  };
  query = devPackagesQuery // { };
  scope = opamNix.buildOpamProject' {} (thunkSource ./ocaml-forester) query;
  main = scope.forester;
  devPackages = builtins.attrValues
    (pkgs.lib.getAttrs (builtins.attrNames devPackagesQuery) scope);
in
{
    forester = main;
}