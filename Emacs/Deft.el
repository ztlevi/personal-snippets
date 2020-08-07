(defun ztlevi-org/post-init-deft ()
  (setq deft-recursive t)
  (setq deft-directory deft-dir)

  ;; deft key prefix is C-c
  (with-eval-after-load 'deft
    (define-key deft-mode-map (kbd "s-w") 'quit-window)
    (define-key deft-mode-map (kbd "C-w") 'deft-filter-decrement-word)
    (define-key deft-mode-map (kbd "C-k") 'deft-filter-clear)
    (define-key deft-mode-map (kbd "s-<backspace>") 'deft-filter-clear)))

(defvar deft-dir ""
"deft org files locaiton")

(setq deft-dir "~/Dropbox/Org-Notes")
