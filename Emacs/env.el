;; Load path from zsh login shell
(when (or IS-LINUX IS-MAC)
  (defvar zsh-executable  "/usr/bin/env zsh")
  (let* ((zshpath (shell-command-to-string
                   (concat zsh-executable " -lc 'printenv PATH'")))
         (pathlst (split-string zshpath ":")))
    (setq exec-path pathlst)
    (setq eshell-path-env zshpath)
    (setenv "PATH" zshpath))

  ;; use zsh as default shell
  (setenv "SHELL" "zsh"))



