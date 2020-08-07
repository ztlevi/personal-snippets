;; Header line
;; http://emacsredux.com/blog/2014/04/05/which-function-mode/
;; (which-function-mode)
;; when editing js file, this feature is very useful
;; (setq-default header-line-format
;;               '((which-func-mode ("" which-func-format " "))))

(defun zt/set-header-line ()
  (interactive)
  (which-function-mode t)
  (spaceline-toggle-all-the-icons-which-function-off)
  (setq header-line-format
        '((which-func-mode ("   " which-func-format " ")))))
(spacemacs/add-to-hooks 'zt/set-header-line '(prog-mode-hook
                                              text-mode-hook
                                              web-mode-hook
                                              ))

(defun zt/scroll-half-up ()
  (interactive)
  (let ((count (/ (1- (window-height)) 2)))
    (evil-scroll-up (- count 1))
    (scroll-down 1)))
(defun zt/scroll-half-down ()
  (interactive)
  (let ((count (/ (1- (window-height)) 2)))
    (evil-scroll-down (+ count 1))
    (scroll-down 1)))
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-u") 'zt/scroll-half-up)
  (define-key evil-normal-state-map (kbd "C-d") 'zt/scroll-half-down)
  (define-key evil-visual-state-map (kbd "C-u") 'zt/scroll-half-up)
  (define-key evil-visual-state-map (kbd "C-d") 'zt/scroll-half-down))

(custom-set-faces
 '(header-line ((t (:inherit font-lock-preprocessor-face)))))
