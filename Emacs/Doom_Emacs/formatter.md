# formatter

```
(package! py-autopep8)
(package! prettier-js)

(def-package! py-autopep8
  :hook (python-mode . py-autopep8-enable-on-save))

(def-package! prettier-js
  :commands prettier-js-mode
  :init
  (add-hook! '(js2-mode-hook
               typescript-mode-hook
               ;; typescript-tsx-mode-hook
               rjsx-mode-hook
               json-mode-hook
               css-mode-hook
               web-mode-hook
               markdown-mode-hook
               gfm-mode-hook)
    #'prettier-js-mode)
  )
```
