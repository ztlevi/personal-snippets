# Smartparens

```emacs-lisp
  ;; smartparens locally disable <
  (after! smartparens
    (sp-with-modes '(c-mode c++-mode objc-mode)
      (sp-local-pair "<" nil :actions nil)))
```
