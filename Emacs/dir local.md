## https://www.gnu.org/software/emacs/manual/html_node/elisp/Directory-Local-Variables.html

## M-x add-dir-local-variable

## Without a .dir-locals.el file

You can also configure directory local variables purely in elisp (i.e. without using a .dir-local.el file). The
following example uses TrampMode paths, and therefore requires directory variable support for remote files to have been
enabled (see below).

```emacs-lisp
    (dir-locals-set-class-variables
     'plone-core
     '((nil . ((buffer-read-only . t)))))

    (dir-locals-set-class-variables
     'plone-instance
     '((nil . ((indent-tabs-mode . nil)
               (fill-column . 80)))
       ;; (python-mode . (()))
       ))

    (dir-locals-set-directory-class
     "/scpc:(user)@(host):/home/(user)/Plone/" 'plone-core)

    (dir-locals-set-directory-class
     "/scpc:(user)@(host):/home/(user)/Plone/zinstance/" 'plone-instance)
```

## cmake project example

```emacs-lisp
((nil
  (projectile-project-compilation-cmd . "mkdir -p Debug && cd Debug && cmake .. && make")
  (projectile-project-run-cmd . "./Debug/cpp_test")
  (projectile-project-test-cmd . "ctest")))
```

## python project example

Enable the virtual environment

```emacs-lisp
((nil (conda-project-env-name . "cs231n")))
```

**Note**: you can easily add a directory local variable with ~SPC i v~.

## Detail

Get the detail information by execute the following function.

```emacs-lisp
(info "(emacs) Directory Variables")
```

This constant is the name of the file where Emacs expects to find the directory-local variables. The name of the file is
.dir-locals.el1. A file by that name in a directory causes Emacs to apply its settings to any file in that directory or
any of its subdirectories (optionally, you can exclude subdirectories; see below). If some of the subdirectories have
their own .dir-locals.el files, Emacs uses the settings from the deepest file it finds starting from the file's
directory and moving up the directory tree. The file specifies local variables as a specially formatted list; see
Per-directory Local Variables, for more details.

Examples:

```emacs-lisp
;;; Directory Local Variables
;;; See Info node `(emacs) Directory Variables' for more information.

((nil
  (require-final-newline . t)
  ;; not tabs in code
  (indent-tabs-mode)
  ;; checkdoc, don't botch English grammar
  (checkdoc-arguments-in-order-flag . nil)
  ;; checkdoc, we don't want docs for internal vars
  (checkdoc-force-docstrings-flag . nil))
 (emacs-lisp-mode
  ;; checkdoc, one space is enough
  (sentence-end-double-space . nil)
  ;; remove trailing whitespace
  (eval . (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))))
```

```emacs-lisp
;;; Directory Local Variables
;;; See Info node `(emacs) Directory Variables' for more information.

((nil
  (sentence-end-double-space . t)
  (require-final-newline . t)
  (indent-tabs-mode))
 (emacs-lisp-mode
  (byte-compile-warnings . (not cl-functions))
  (whitespace-style face tabs trailing lines-tail)
  (whitespace-line-column . 80)
  (eval ignore-errors
        "Write-contents-functions is a buffer-local alternative to before-save-hook"
        (add-hook 'write-contents-functions
                  (lambda ()
                    (delete-trailing-whitespace)
                    nil))
        (require 'whitespace)
        "Sometimes the mode needs to be toggled off and on."
        (whitespace-mode 0)
        (whitespace-mode 1))))
```
