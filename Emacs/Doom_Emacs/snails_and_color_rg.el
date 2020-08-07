(when IS-MAC
  (package! exec-path-from-shell))
(package! color-rg :recipe (:host github :repo "manateelazycat/color-rg"))
(package! snails :recipe (:host github :repo "manateelazycat/snails"))
(package! fuz :recipe (:host github :repo "rustify-emacs/fuz.el"))

(use-package! color-rg
  :config
  ;; https://emacs.stackexchange.com/a/10588/22102
  (evil-make-overriding-map color-rg-mode-map 'normal)
  ;; force update evil keymaps after git-timemachine-mode loaded
  (add-hook 'color-rg-mode-hook #'evil-normalize-keymaps)

  (map!
   (:after color-rg
     (:map color-rg-mode-map
       "j" nil "k" nil "l" nil "h" nil
       "C-k" #'color-rg-jump-prev-keyword
       "C-j" #'color-rg-jump-next-keyword
       :nv "gr" #'color-rg-rerun)))

  (custom-set-faces!
    `(color-rg-font-lock-match :foreground ,(doom-color 'red))
    `(color-rg-font-lock-header-line-text :foreground ,(doom-color 'dark-cyan))
    `(color-rg-font-lock-function-location :foreground ,(doom-color 'magenta))
    `(color-rg-font-lock-header-line-keyword :foreground ,(doom-color 'magenta))
    `(color-rg-font-lock-header-line-edit-mode :foreground ,(doom-color 'magenta))))


(when (display-graphic-p)
  (use-package! snails
    :load-path  "~/.emacs.d/.local/straight/repos/snails"
    :config
    (setq snails-input-buffer-text-scale 1)
    (set-evil-initial-state!
      '(snails-mode)
      'insert)
    (map!
     (:map snails-mode-map
       :nvi "C-g" #'snails-quit
       :nvi "ESC ESC ESC" #'snail-quit
       :nvi "C-n" #'snails-select-next-item
       :nvi "C-p" #'snails-select-prev-item
       :nvi "C-v" #'snails-select-next-backend
       :nvi "M-v" #'snails-select-prev-backend
       :nvi "RET" #'snails-candidate-do
       :nvi "C-RET" #'snails-candiate-alternate-do))
    )

  (use-package! fuz
    :config
    (unless (require 'fuz-core nil t)
      (fuz-build-and-load-dymod))))


;; autoloads/misc.el
;;;###autoload
(defun +my/search-project ()
  (interactive)
  (if current-prefix-arg
      (color-rg-search-project)
    (+default/search-project)))
