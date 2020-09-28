https://github.com/hlissner/doom-emacs/blob/develop/docs/getting_started.org#package-management

## Package management

Doom’s package manager is declarative. Your `DOOMDIR` is a module, and modules may optionally
possess a packages.el file, where you may declare what packages you want to install (and where from)
using the `package!` macro. It can be used to:

1.  Install packages (conditionally, even),
2.  Disable packages (uninstalling them and disabling their configuration),
3.  Or change where a package is installed from.

If a package is installed via ELPA and does not have a `package!` declaration, Doom will assume the
package is unwanted and uninstall it for you next time `doom refresh` is executed.

> Remember to run `doom refresh` after modifying your packages, to ensure they are installed and
> properly integrated into Doom.

### Installing packages

To install a package, add a `package!` declaration for it to `DOOMDIR/packages.el`:

```emacs-lisp
;; Install a package named "example" from ELPA or MELPA
(package! example)

;; Tell Doom to install it from a particular archive (e.g. elpa). By default, it
;; will search orgmode.org and melpa.org before searching elpa.gnu.org. See
;; `package-archives' to adjust this order (or to see what values :pin will
;; accept).
(package! example :pin "elpa")

;; Instruct Doom to install this package once, but never update it when you run
;; `doom update` or `doom upgrade`:
(package! example :freeze t)

;; Or tell Doom to not manage a particular package at all.
(package! example :ignore t)
```

`package!` will return non-nil if the package isn’t disabled and is cleared for install. Use this
fact to conditionally install other packages, e.g.

```emacs-lisp
(when (package! example)
  (package! plugin-that-example-depends-on))
```

### Installing packages from external sources

To install a package straight from an external source (like github, gitlab, etc), you’ll need to
specify a [MELPA-style straight recipe](https://github.com/raxod502/straight.el#the-recipe-format):

Here are a few examples:

```emacs-lisp
;; Install it directly from a github repository. For this to work, the package;; must have an appropriate .el and must have at least a Package-Version ;; or Version line in its header.
(package! example :recipe (:host github :repo "username/my-example-fork"))

;; If the source files for a package are in a subdirectory in said repo, you'll
;; need to specify what files to pull in.
(package! example :recipe
  (:host github
   :repo "username/my-example-fork"
   :files ("*.el" "src/lisp/*.el")))

;; To grab a particular commit:
(package! example :recipe
  (:host gitlab
   :repo "username/my-example-fork"
   :branch "develop"))

;; If a package has a default recipe on MELPA or emacsmirror, you may omit
;; keywords and the recipe will inherit from their original.
(package! example :recipe (:branch "develop"))

;; If the repo pulls in many unneeded submodules, you can disable recursive cloning
(package! example :recipe (:nonrecursive t))
```

### Disabling packages

The `package!` macro possesses a `:disable` property.

```emacs-lisp
(package! irony :disable t)
(package! rtags :disable t)
```

Once a package is disabled, `use-packages!` and `after!` blocks for it will be ignored, and the
package will be removed the next time you run `doom refresh`. Use this to disable undesirable
packages included with the built-in modules.

Alternatively, the `disable-packages!` macro exists for more concisely disabling multiple packages:

```emacs-lisp
(disable-packages! irony rtags)
```

### Changing a built-in recipe for a package

If a module installs package X, but you’d like to install it from somewhere else (say, a superior
fork or a fork with a bugfix), simple add a `package!` declaration for it in your
`DOOMDIR/packages.el`. Your private declarations always have precedence over modules (even your own
modules).

```emacs-lisp
;; modules/editor/evil/packages.el
(package! evil) ; installs from MELPA

;; DOOMDIR/packages.el
(package! evil :recipe (:host github :repo "username/my-evil-fork"))
```

You will need to run `doom refresh` for this change to take effect.

### Using/loading local packages

```emacs-lisp
(def-package! ccls
  :load-path "~/Dev/Emacs/emacs-ccls"
  )
```
