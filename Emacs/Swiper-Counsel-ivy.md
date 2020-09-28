<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->

**Table of Contents**

- [Counsel](#counsel)
  - [Counsel args (file mask)](#counsel-args-file-mask)
    - [Purpose](#purpose)
    - [UI](#ui)
    - [File Mask](#file-mask)
- [Swiper](#swiper)
  - [Swiper-avy](#swiper-avy)
- [Ivy](#ivy)
  - [Ivy dispatching done](#ivy-dispatching-done)

<!-- markdown-toc end -->

# Counsel

## Counsel args (file mask)

https://github.com/abo-abo/swiper/pull/1559

### Purpose

Enable users to interactively specify extra switches along with the search pattern when invoking
counsel-ag.

For example, `M-x counsel-ag`:

```
[~/.emacs.d] ag: --elisp --ignore site-lisp -- let
```

### UI

Assume `counsel-ag-command` is set to `ag --nocolor --nogroup`.

- It behaves the same as before when the query starts with anything but a dash: `M-x counsel-ag abc`
  ↵ Produces `ag --nocolor --nogroup abc`

- Regex are escaped: `M-x counsel-ag abc[^d].` ↵ Produces `ag --nocolor --nogroup abc\[\^d\].`

- Starting with `-` or `--` passes the text as switches until a `--`:
  `M-x counsel-ag --elisp -- abc` ↵ Produces `ag --nocolor --nogroup --elisp abc`

- Initial dashes need to be escaped if they are not switches: `M-x counsel-ag \-abc` ↵ Produces
  `ag --nocolor --nogroup \\-abc`

- Switches are not escaped. Queries are: `M-x counsel-ag --elisp -- abc[^d].` ↵ Produces
  `ag --nocolor --nogroup --elisp abc\[\^d\].`

- Using a prefix argument behaves the same as before:
  `C-u M-x counsel-ag ag --nocolor --nogroup --elisp` ↵ `abc` ↵ Produces
  `ag --nocolor --nogroup --elisp abc`

### File Mask

- `-t` can apply builtin file masks to filter files, use `rg --type-list` to see all the builtin
  masks, e.g. `M-x counsel-rg ↵ -t txt -- list` to search "list" in all txt files.
- `--type-add` can help to define custom file mask, e.g.
  `--type-add 'foo:*.{foo,boo}' -t foo PATTERN`.
- Or, we can define custom search functions using the following format:
  ```
  (counsel-rg &optional INITIAL-INPUT INITIAL-DIRECTORY EXTRA-RG-ARGS RG-PROMPT)
  ```

# Swiper

## Swiper-avy

In 2015, `swiper-avy` was [added](https://oremacs.com/2015/05/23/swiper-0.5.0/), which could also be
used as a workaround for many candidates on a single line. Press

<kbd>C-'</kbd> to visually select any candidate on screen using
[avy](https://github.com/abo-abo/avy).

# Ivy

## Ivy dispatching done

After open a ivy buffer, call `M-o ivy-posframe-dispatching-done` to dispatching actions.

Example `M-x counsel-colors-web` -> `M-o` -> `H` to insert hex value

Actions can be set via `ivy-set-acions`:

```emacs-lisp
(ivy-set-actions
 'counsel-colors-web
 '(("h" counsel-colors-action-insert-hex "insert hexadecimal value")
   ("H" counsel-colors-action-kill-hex "kill hexadecimal value")))
```

## ivy-initial-inputs-alist

```emacs-lisp
  (setq ivy-initial-inputs-alist nil
        ivy-format-function (quote ivy-format-function-arrow)
        ivy-re-builders-alist '((counsel-rg . ivy--regex-plus)
                                (counsel-grep . ivy--regex-plus)
                                (swiper . ivy--regex-plus)
                                (t . ivy--regex-ignore-order)))
```
