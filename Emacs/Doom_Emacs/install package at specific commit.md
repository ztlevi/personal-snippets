# Install package at specific commit

Example:

```
(package! ghub+ :recipe (:fetcher github :repo "vermiculus/ghub-plus" :commit "f36334b"))

(package! magit :recipe (:fetcher github :repo "magit/magit" :files ("lisp/*") :commit "9b42fb4"))
```
