# Flake

## References

https://www.tweag.io/blog/2020-05-25-flakes/

## List inputs

`dwarffs`’s `flake.nix` file declares an explicit dependency on Nixpkgs, which is also a
[flake](https://github.com/NixOS/nixpkgs/blob/master/flake.nix). We can see the dependencies of a
flake as follows:

```sh
$ nix flake list-inputs github:edolstra/dwarffs
github:edolstra/dwarffs/d11b181af08bfda367ea5cf7fad103652dc0409f
├───nix: github:NixOS/nix/3aaceeb7e2d3fb8a07a1aa5a21df1dca6bbaa0ef
│   └───nixpkgs: github:NixOS/nixpkgs/b88ff468e9850410070d4e0ccd68c7011f15b2be
└───nixpkgs: github:NixOS/nixpkgs/b88ff468e9850410070d4e0ccd68c7011f15b2be
```

For local repo:

```sh
$ nix flake list-inputs ~/.config/dotfiles
```

## Flake outputs

Another goal of flakes is to provide a standard structure for discoverability within Nix-based
projects. Flakes can provide arbitrary Nix values, such as packages, NixOS modules or library
functions. These are called its _outputs_. We can see the outputs of a flake as follows:

```language-console
$ nix flake show github:edolstra/dwarffs
github:edolstra/dwarffs/d11b181af08bfda367ea5cf7fad103652dc0409f
├───checks
│   ├───aarch64-linux
│   │   └───build: derivation 'dwarffs-0.1.20200409'
│   ├───i686-linux
│   │   └───build: derivation 'dwarffs-0.1.20200409'
│   └───x86_64-linux
│       └───build: derivation 'dwarffs-0.1.20200409'
├───defaultPackage
│   ├───aarch64-linux: package 'dwarffs-0.1.20200409'
│   ├───i686-linux: package 'dwarffs-0.1.20200409'
│   └───x86_64-linux: package 'dwarffs-0.1.20200409'
├───nixosModules
│   └───dwarffs: NixOS module
└───overlay: Nixpkgs overlay
```

While a flake can have arbitrary outputs, some of them, if they exist, have a special meaning to
certain Nix commands and therefore must have a specific type. For example, the output
`defaultPackage.<system>` must be a derivation; it’s what `nix build` and `nix shell` will build by
default unless you specify another output. The `nix` CLI allows you to specify another output
through a syntax reminiscent of URL fragments:

```language-console
$ nix build github:edolstra/dwarffs#checks.aarch64-linux.build
```

By the way, the standard `checks` output specifies a set of derivations to be built by a continuous
integration system such as Hydra. Because flake evaluation is hermetic and the lock file locks all
dependencies, it’s guaranteed that the `nix build` command above will evaluate to the same result as
the one in the CI system.

## The flake registry

Flake locations are specified using a URL-like syntax such as `github:edolstra/dwarffs` or
`git+https://github.com/NixOS/patchelf`. But because such URLs would be rather verbose if you had to
type them all the time on the command line, there also is a
[flake registry](https://raw.githubusercontent.com/NixOS/flake-registry/master/flake-registry.json)
that maps symbolic identifiers such as `nixpkgs` to actual locations like
`https://github.com/NixOS/nixpkgs`. So the following are (by default) equivalent:

```language-console
$ nix shell nixpkgs#cowsay --command cowsay Hi!
$ nix shell github:NixOS/nixpkgs#cowsay --command cowsay Hi!
```

It’s possible to override the registry locally. For example, you can override the `nixpkgs` flake to
your own Nixpkgs tree:

```language-console
$ nix registry add nixpkgs ~/my-nixpkgs
```

or pin it to a specific revision:

```language-console
$ nix registry add nixpkgs github:NixOS/nixpkgs/5272327b81ed355bbed5659b8d303cf2979b6953
```

## Writing your first flake

Unlike Nix channels, creating a flake is pretty simple: you just add a `flake.nix` and possibly a
`flake.lock` to your project’s repository. As an example, suppose we want to create our very own
Hello World and distribute it as a flake. Let’s create this project first:

```language-console
$ git init hello
$ cd hello
$ echo 'int main() { printf("Hello World"); }' > hello.c
$ git add hello.c
```

To turn this Git repository into a flake, we add a file named `flake.nix` at the root of the
repository with the following contents:

```language-nix
{
  description = "A flake for building Hello World";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-20.03;

  outputs = { self, nixpkgs }: {

    defaultPackage.x86_64-linux =
      # Notice the reference to nixpkgs here.
      with import nixpkgs { system = "x86_64-linux"; };
      stdenv.mkDerivation {
        name = "hello";
        src = self;
        buildPhase = "gcc -o hello ./hello.c";
        installPhase = "mkdir -p $out/bin; install -t $out/bin hello";
      };

  };
}
```

The command `nix flake init` creates a basic `flake.nix` for you.

Note that any file that is not tracked by Git is invisible during Nix evaluation, in order to ensure
hermetic evaluation. Thus, you need to make `flake.nix` visible to Git:

```language-console
$ git add flake.nix
```

Let’s see if it builds!

```language-console
$ nix build
warning: creating lock file '/home/eelco/Dev/hello/flake.lock'
warning: Git tree '/home/eelco/Dev/hello' is dirty

$ ./result/bin/hello
Hello World
```

or equivalently:

```language-console
$ nix shell --command hello
Hello World
```

It’s also possible to get an interactive development environment in which all the dependencies (like
GCC) and shell variables and functions from the derivation are in scope:

```language-console
$ nix dev-shell
$ eval "$buildPhase"
$ ./hello
Hello World
```

So what does all that stuff in `flake.nix` mean?

- The `description` attribute is a one-line description shown by `nix flake info`.
- The `inputs` attribute specifies other flakes that this flake depends on. These are fetched by Nix
  and passed as arguments to the `outputs` function.
- The `outputs` attribute is the heart of the flake: it’s a function that produces an attribute set.
  The function arguments are the flakes specified in `inputs`.

  The `self` argument denotes _this_ flake. Its primarily useful for referring to the source of the
  flake (as in `src = self;`) or to other outputs (e.g. `self.defaultPackage.x86_64-linux`).

- The attributes produced by `outputs` are arbitrary values, except that (as we saw above) there are
  some standard outputs such as `defaultPackage.${system}`.
- Every flake has some metadata, such as `self.lastModifiedDate`, which is used to generate a
  version string like `hello-20191015`.

You may have noticed that the dependency specification `github:NixOS/nixpkgs/nixos-20.03` is
imprecise: it says that we want to use the `nixos-20.03` branch of Nixpkgs, but doesn’t say which
Git revision. This seems bad for reproducibility. However, when we ran `nix build`, Nix
automatically generated a lock file that precisely states which revision of `nixpkgs` to use:

```language-console
$ cat flake.lock
{
  "nodes": {
    "nixpkgs": {
      "info": {
        "lastModified": 1587398327,
        "narHash": "sha256-mEKkeLgUrzAsdEaJ/1wdvYn0YZBAKEG3AN21koD2AgU="
      },
      "locked": {
        "owner": "NixOS",
        "repo": "nixpkgs",
        "rev": "5272327b81ed355bbed5659b8d303cf2979b6953",
        "type": "github"
      },
      "original": {
        "owner": "NixOS",
        "ref": "nixos-20.03",
        "repo": "nixpkgs",
        "type": "github"
      }
    },
    "root": {
      "inputs": {
        "nixpkgs": "nixpkgs"
      }
    }
  },
  "root": "root",
  "version": 5
}
```

Any subsequent build of this flake will use the version of `nixpkgs` recorded in the lock file. If
you add new inputs to `flake.nix`, when you run any command such as `nix build`, Nix will
automatically add corresponding locks to `flake.lock`. However, it won’t replace existing locks. If
you want to update a locked input to the latest version, you need to ask for it:

```language-console
$ nix flake update --update-input nixpkgs
$ nix build
```

To wrap things up, we can now commit our project and push it to GitHub, after making sure that
everything is in order:

```language-console
$ nix flake check
$ git commit -a -m 'Initial version'
$ git remote add origin git@github.com:edolstra/hello.git
$ git push -u origin master
```

Other users can then use this flake:

```language-console
$ nix shell github:edolstra/hello -c hello
```
