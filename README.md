# Agda backend for [Forester](https://www.forester-notes.org/)

This is an experimental project implementing a backend to export
literate agda to forester trees.

## Building

The project is compatible with Forester 5, and bundles Agda nightly version
f369741 with it.

### With nix

If you have nix installed, the simplest way to build and run this tool is via
`nix-build` or `nix-shell`. Running `nix-shell` will put you in an environment
with access to the `agda-forester` command.

### Without nix

To use this project without nix, you will need to first have
 [cabal](https://www.haskell.org/cabal/) and
 [treelist](https://github.com/samtoth/treelist) installed. This is a small
utility to help generate correct links in the Agda code. Then clone the repo and
install with `cabal install`.

## Usage

Example usage:

```
agda-forester --forest -o trees/agda src/Everything.agda
```

Will compile all `.lagda.tree` files to `.tree` files and place them in the
`trees/agda` directory, and compile all `.agda` files to html using the default
backend and place them (by default) in the `assets/html` directory.

## Project setup

The recommended way to structure your project is with a source directory
containing agda code, seperate to the forester src folder.

It is recommended to use the theme at
[github:samtoth/forest-theme/tree/agda-forester](https://github.com/samtoth/forest-theme/tree/agda-forester),
which provides the `Agda.css` file.

You will also require that the macros defined in
[./macros.tree](./macros.tree) are imported somewhere in each literate tree
file.

For an example of how to structure a project check out
[this project](https://github.com/samtoth/agda-synthetic-categories)

## Options

| Option                         | Description                                                                                                                                                                                                                                                                                                                                                                                                 | Default       |
| ------------------------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------- |
| `--forest`                     | Enable the agda-forester backend                                                                                                                                                                                                                                                                                                                                                                            |               |
| `-o DIR` or `--forest-dir DIR` | Directory in which to write generated .tree files                                                                                                                                                                                                                                                                                                                                                           | `trees/`      |
| `--fhtml-dir DIR`              | Directory in which to write generated .html files (for non-literate code)                                                                                                                                                                                                                                                                                                                                   | `assets/html` |
| `--fhtml-link-root PATH`       | Root path of links to HTML modules                                                                                                                                                                                                                                                                                                                                                                          | `/html/`      |
| `--fforest-root PATH`          | Path to root of Forest. This should correspond to the url in your `forest.toml` file, without the domain. E.g. if `url = "https://samtoth.github.io/agda-synthetic-categories/"`, then the forest root should be set to `/agda-synthetic-categories/`.                                                                                                                                                      | `/`           |
| `--fhtml-css-path PATH`        | Path to agda.css file                                                                                                                                                                                                                                                                                                                                                                                       | `Agda.css`    |
| `--fdisable-backlinks`         | By default agda-forester generates "internal" links to other modules in the generated output, this means the 'backlink' and 'related' portions of the subtree will become populated. If the codebase is large this can create extremely large Forests which are slow to build. Enabling this option causes agda-forester to generate "external" links which do not suffer from the afformentioned drawbacks |               |

## Enabling links

~~Currently, in order to get links to work, you need to annotate your
literate tree files with meta commands.~~

Links now work automatically :)
