# cspell

```emacs-lisp

  (flycheck-def-config-file-var flycheck-cspelljson cspell ".cspell.json"
    :safe #'stringp)
  (flycheck-define-checker cspell
    "Cspell checker supports camel case spell checking."
    :command ("cspell"
              (config-file "--config" flycheck-cspelljson)
              source-inplace)
    :error-patterns
    ((info line-start (file-name) ":" line ":" column " - "
           (message)
           line-end))
    :modes (c-mode c++-mode js2-mode rjsx-mode typescript-mode web-mode java-mode go-mode))
  (add-to-list 'flycheck-checkers 'cspell)

```
