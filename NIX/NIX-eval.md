# Eval

```
 To evaluate a Nix expression given on the command line:
  $ nix eval --expr '1 + 2'

  To evaluate a Nix expression from a file or URI:
  $ nix eval -f ./my-nixpkgs hello.name

  To get the current version of Nixpkgs:
  $ nix eval --raw "nixpkgs#lib.version"

  To print the store path of the Hello package:
  $ nix eval --raw nixpkgs#hello

  To get a list of checks in the 'nix' flake:
  $ nix eval nix#checks.x86_64-linux --apply builtins.attrNames
```
