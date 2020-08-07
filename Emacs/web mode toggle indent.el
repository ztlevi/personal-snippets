(spacemacs/set-leader-keys-for-major-mode 'js2-mode
  "ti" 'my-toggle-web-indent)
(spacemacs/set-leader-keys-for-major-mode 'react-mode
  "ti" 'my-toggle-web-indent)
(spacemacs/set-leader-keys-for-major-mode 'web-mode
  "ti" 'my-toggle-web-indent)
(spacemacs/set-leader-keys-for-major-mode 'css-mode
  "ti" 'my-toggle-web-indent)
(spacemacs/declare-prefix-for-mode 'js2-mode "mt" "toggle")
(spacemacs/declare-prefix-for-mode 'react-mode "mt" "toggle")
(spacemacs/declare-prefix-for-mode 'web-mode "mt" "toggle")
(spacemacs/declare-prefix-for-mode 'css-mode "mt" "toggle")

(defun my-toggle-web-indent ()
  (interactive)
  ;; web development
  (if (eq major-mode 'json-mode)
      (progn
        (setq js-indent-level (if (= js-indent-level 2) 4 2))))

  (if (or (eq major-mode 'js-mode) (eq major-mode 'js2-mode))
      (progn
        (setq js-indent-level (if (= js-indent-level 2) 4 2))))

  (if (eq major-mode 'web-mode)
      (progn (setq web-mode-markup-indent-offset (if (= web-mode-markup-indent-offset 2) 4 2))
             (setq web-mode-css-indent-offset (if (= web-mode-css-indent-offset 2) 4 2))
             (setq web-mode-code-indent-offset (if (= web-mode-code-indent-offset 2) 4 2))))
  (if (eq major-mode 'css-mode)
      (setq css-indent-offset (if (= css-indent-offset 2) 4 2)))

  (setq indent-tabs-mode nil))