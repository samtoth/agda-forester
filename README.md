 # Agda backend for [Forester](https://www.forester-notes.org/)

This is an experimental project implementing a backend to export
literate agda to forrester trees.

## Building

The project depends on agda, and requires that forester version 4.3 is on the path (to query the trees).

If you have nix installed, the most simple way to build and run this tool is via `nix-build` or `nix-shell`. Running `nix-shell` will put you in an environment with access to the `agda-forester` command.

## Usage

Example usage:

```
agda-forester --forest -o trees/agda -S1 src/Everything.agda 
```

Will compile all `.agda` and `.lagda.tree` files to `.tree` files and place them in the `trees/agda` directory.

## Project setup

The recommended way to structure your project is with a source directory containing agda code, seperate to the forester src folder.

The backend requires that you have another tree settings file called
`proxy.toml` at the top level. This is needed to proxy the .lagda trees to discover what links to add.

You will also require that the macros defined in [./macros.tree](./macros.tree) are imported somewhere in each literate tree file.

For the code to be styled correctly, you also need to have the `Agda.css` file included in your header somewhere. 

For an example of how to structure a project check out [this project](https://github.com/samtoth/synthetic-agda)

## Enabling links

Currently, in order to get links to work, you need to annotate your
literate tree files with meta commands.

For a tree that contains the agda definitions `zero`, `suc` and `_+_`, you would add the following command at the top of the tree:

```
\meta{defines}{\startverb ["zero","suc","_+_"] \stopverb}
```

In addition, to get module links working, you need to add the following to the top level tree:
```
\meta{defines}{\startverb ["MODULENAME"] \stopverb}
```

## Structure flag

The original plan for this project was to take plain non-literate agda files and convert them into structured trees, with subtrees corresponding to each definition and submodules etc. 

For various reasons, this turned out to be very difficult to implement. Mainly there are many edge cases around what agda treats as a module and a definition - and these don't really correspond to a chunk of code in a meaningful way in general.

The partial implementation of this automatic tree generation is still left in the project and can be activated by adding the flag `-S2` to the command invocation - but there is no promise that it will generate sensible output for anything but the most simple agda code.

Currently the default way to deal with non literate agda files is to simply use the html backend. This corresponds to the flag `-S0` which is set by default.

## Future plans

Some potential ideas for augmenting the forester output:

 - [ ] There is a plan to implement a halfway house strategy (enabled with `-S1`), where we generate a module tree file which contains the full source of the module, with meta-define blocks autogenerated. This has a number of benifits compared to plain html output - for example: enabling the backlink/related sections in the forester output; and enabling search of particular functions.
 - [ ] Add meta-defines to ninja-keys search
 - [ ] Auto generate the meta-defines to reduce the burdon of writing literate files
 - [ ] Auto generate type information as forest tree metadata
 - [ ] Implement some kind of type directed search using the above type information. (In particular, maybe translating types into agda-unimath style names that can be searched)