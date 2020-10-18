# Nix command/repl

<!-- bodyContent -->

From NixOS Wiki

    				Jump to:					[navigation](https://nixos.wiki/wiki/Nix_command/repl#mw-navigation), 					[search](https://nixos.wiki/wiki/Nix_command/repl#p-search)

This article is about the `nix repl` subcommand.

## Description

`nix repl` is an interactive shell to explore the Nix language as well as configurations,options and
packages in [Nixpkgs](https://nixos.wiki/wiki/Nixpkgs "Nixpkgs"). It supports tab-completion.

## Usage

```sh
$ nix repl FLAGS... FILES...
```

## Flags

- `--arg NAME EXPR` argument to be passed to Nix functions
- `--argstr NAME STRING` string-valued argument to be passed to Nix functions
- `--impure` allow access to mutable paths and repositories
- `-I, --include PATH` add a path to the list of locations used to look up <...> file names
- `--override-flake ORIGINAL-REF RESOLVED-REF` override a flake registry value

## Examples

If you run `:?` inside a repl instance, it will print out the available commands. A list is given
below (from July 2020)

```sh
nix-repl> :?
The following commands are available:

  <expr>        Evaluate and print expression
  <x> = <expr>  Bind expression to variable
  :a <expr>     Add attributes from resulting set to scope
  :b <expr>     Build derivation
  :i <expr>     Build derivation, then install result into current profile
  :l <path>     Load Nix expression and add it to scope
  :p <expr>     Evaluate and print expression recursively
  :q            Exit nix-repl
  :r            Reload all files
  :s <expr>     Build dependencies of derivation, then start nix-shell
  :t <expr>     Describe result of evaluation
  :u <expr>     Build derivation, then start nix-shell
```

```sh
$ nix repl '<nixpkgs>'
Added 8013 variables.
nix-repl> pkgs.<tab><tab>
Display all 8013 possibilities? (y or n)
# load your systems nixos configuration
nix-repl> :l <nixpkgs/nixos>
Added 5 variables.
nix-repl> config.<tab><tab>
...
nix-repl> config.services.nginx.enable
false
# download/build package and open in a nix-shell
nix-repl> :u sl
[nix-shell:~]$ sl # choo choo...
```

## See also

- [Nix command](https://nixos.wiki/wiki/Nix_command "Nix command")
