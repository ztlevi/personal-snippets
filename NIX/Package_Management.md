## 6.1. Declarative Package Management[](https://nixos.org/nixos/manual/index.html#sec-declarative-package-mgmt)

With declarative package management, you specify which packages you want on your system by setting
the option
[`environment.systemPackages`](https://nixos.org/nixos/manual/options.html#opt-environment.systemPackages).
For instance, adding the following line to `configuration.nix` enables the Mozilla Thunderbird email
application:

```nix
environment.systemPackages = [ pkgs.thunderbird ];
```

The effect of this specification is that the Thunderbird package from Nixpkgs will be built or
downloaded as part of the system when you run **nixos-rebuild switch**.

**Note:** Some packages require additional global configuration such as D-Bus or systemd service
registration so adding them to
[`environment.systemPackages`](https://nixos.org/nixos/manual/options.html#opt-environment.systemPackages)
might not be sufficient. You are advised to check the
[list of options](https://nixos.org/nixos/manual/options.html "Appendix A. Configuration Options")
whether a NixOS module for the package does not exist.

You can get a list of the available packages as follows:

```prompt
$ nix-env -qaP '*' --description
```

nixos.firefox firefox-23.0 Mozilla Firefox - the browser, reloaded _`...`_

The first column in the output is the _attribute name_, such as `nixos.thunderbird`.

Note: the `nixos` prefix tells us that we want to get the package from the `nixos` channel and works
only in CLI tools. In declarative configuration use `pkgs` prefix (variable).

To “uninstall” a package, simply remove it from
[`environment.systemPackages`](https://nixos.org/nixos/manual/options.html#opt-environment.systemPackages)
and run **nixos-rebuild switch**.

### 6.1.1. Customising Packages[](https://nixos.org/nixos/manual/index.html#sec-customising-packages)

Some packages in Nixpkgs have options to enable or disable optional functionality or change other
aspects of the package. For instance, the Firefox wrapper package (which provides Firefox with a set
of plugins such as the Adobe Flash player) has an option to enable the Google Talk plugin. It can be
set in `configuration.nix` as follows: `nixpkgs.config.firefox.enableGoogleTalkPlugin = true;`

**Warning:** Unfortunately, Nixpkgs currently lacks a way to query available configuration options.

Apart from high-level options, it’s possible to tweak a package in almost arbitrary ways, such as
changing or disabling dependencies of a package. For instance, the Emacs package in Nixpkgs by
default has a dependency on GTK 2. If you want to build it against GTK 3, you can specify that as
follows:

```nix
[`environment.systemPackages`](https://nixos.org/nixos/manual/options.html#opt-environment.systemPackages) = [ (pkgs.emacs.override { gtk = pkgs.gtk3; }) ];
```

The function `override` performs the call to the Nix function that produces Emacs, with the original
arguments amended by the set of arguments specified by you. So here the function argument `gtk` gets
the value `pkgs.gtk3`, causing Emacs to depend on GTK 3. (The parentheses are necessary because in
Nix, function application binds more weakly than list construction, so without them,
[`environment.systemPackages`](https://nixos.org/nixos/manual/options.html#opt-environment.systemPackages)
would be a list with two elements.)

Even greater customisation is possible using the function `overrideAttrs`. While the `override`
mechanism above overrides the arguments of a package function, `overrideAttrs` allows changing the
_attributes_ passed to `mkDerivation`. This permits changing any aspect of the package, such as the
source code. For instance, if you want to override the source code of Emacs, you can say:

```nix
[`environment.systemPackages`](https://nixos.org/nixos/manual/options.html#opt-environment.systemPackages) = [
  (pkgs.emacs.overrideAttrs (oldAttrs: {
    name = "emacs-25.0-pre";
    src = /path/to/my/emacs/tree;
  }))
];
```

Here, `overrideAttrs` takes the Nix derivation specified by `pkgs.emacs` and produces a new
derivation in which the original’s `name` and `src` attribute have been replaced by the given values
by re-calling `stdenv.mkDerivation`. The original attributes are accessible via the function
argument, which is conventionally named `oldAttrs`.

The overrides shown above are not global. They do not affect the original package; other packages in
Nixpkgs continue to depend on the original rather than the customised package. This means that if
another package in your system depends on the original package, you end up with two instances of the
package. If you want to have everything depend on your customised instance, you can apply a _global_
override as follows:

```nix
nixpkgs.config.packageOverrides = pkgs:
  { emacs = pkgs.emacs.override { gtk = pkgs.gtk3; };
  };
```

The effect of this definition is essentially equivalent to modifying the `emacs` attribute in the
Nixpkgs source tree. Any package in Nixpkgs that depends on `emacs` will be passed your customised
instance. (However, the value `pkgs.emacs` in `nixpkgs.config.packageOverrides` refers to the
original rather than overridden instance, to prevent an infinite recursion.)

### 6.1.2. Adding Custom Packages[](https://nixos.org/nixos/manual/index.html#sec-custom-packages)

It’s possible that a package you need is not available in NixOS. In that case, you can do two
things. First, you can clone the Nixpkgs repository, add the package to your clone, and (optionally)
submit a patch or pull request to have it accepted into the main Nixpkgs repository. This is
described in detail in the [Nixpkgs manual](https://nixos.org/nixpkgs/manual). In short, you clone
Nixpkgs:

```nix
$ git clone https://github.com/NixOS/nixpkgs
$ cd nixpkgs
```

Then you write and test the package as described in the Nixpkgs manual. Finally, you add it to
`environment.systemPackages`, e.g.

```nix
[`environment.systemPackages`](https://nixos.org/nixos/manual/options.html#opt-environment.systemPackages) = [ pkgs.my-package ];
```

and you run **nixos-rebuild**, specifying your own Nixpkgs tree:

```nix
# nixos-rebuild switch -I nixpkgs=/path/to/my/nixpkgs
```

The second possibility is to add the package outside of the Nixpkgs tree. For instance, here is how
you specify a build of the [GNU Hello](https://www.gnu.org/software/hello/) package directly in
`configuration.nix`:

```nix
[`environment.systemPackages`](https://nixos.org/nixos/manual/options.html#opt-environment.systemPackages) =
  let
    my-hello = with pkgs; stdenv.mkDerivation rec {
      name = "hello-2.8";
      src = fetchurl {
        url = "mirror://gnu/hello/${name}.tar.gz";
        sha256 = "0wqd8sjmxfskrflaxywc7gqw7sfawrfvdxd9skxawzfgyy0pzdz6";
      };
    };
  in
  [ my-hello ];
```

Of course, you can also move the definition of `my-hello` into a separate Nix expression, e.g.

```nix
[`environment.systemPackages`](https://nixos.org/nixos/manual/options.html#opt-environment.systemPackages) = [ (import ./my-hello.nix) ];
```

where `my-hello.nix` contains:

```nix
withimport <nixpkgs> {}; # bring all of Nixpkgs into scope

stdenv.mkDerivation rec {
  name = "hello-2.8";
  src = fetchurl {
    url = "mirror://gnu/hello/${name}.tar.gz";
    sha256 = "0wqd8sjmxfskrflaxywc7gqw7sfawrfvdxd9skxawzfgyy0pzdz6";
  };
}
```

This allows testing the package easily:

```nix
$ nix-build my-hello.nix
$ ./result/bin/hello
Hello, world!
```

## 6.2. Ad-Hoc Package Management[](https://nixos.org/nixos/manual/index.html#sec-ad-hoc-packages)

With the command **nix-env**, you can install and uninstall packages from the command line. For
instance, to install Mozilla Thunderbird:

```nix
$ nix-env -iA nixos.thunderbird
```

If you invoke this as root, the package is installed in the Nix profile
`/nix/var/nix/profiles/default` and visible to all users of the system; otherwise, the package ends
up in `` /nix/var/nix/profiles/per-user/_`username`_/profile `` and is not visible to other users.
The `-A` flag specifies the package by its attribute name; without it, the package is installed by
matching against its package name (e.g. `thunderbird`). The latter is slower because it requires
matching against all available Nix packages, and is ambiguous if there are multiple matching
packages.

Packages come from the NixOS channel. You typically upgrade a package by updating to the latest
version of the NixOS channel:

```nix
$ nix-channel --update nixos
```

and then running `nix-env -i` again. Other packages in the profile are _not_ affected; this is the
crucial difference with the declarative style of package management, where running **nixos-rebuild
switch** causes all packages to be updated to their current versions in the NixOS channel. You can
however upgrade all packages for which there is a newer version by doing:

```nix
$ nix-env -u '*'
```

A package can be uninstalled using the `-e` flag:

```nix
$ nix-env -e thunderbird
```

Finally, you can roll back an undesirable **nix-env** action:

```nix
$ nix-env --rollback
```

**nix-env** has many more flags. For details, see the nix-env(1) manpage or the Nix manual.
