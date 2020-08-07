;;; minimal_package_test.el -*- lexical-binding: t; -*-
;; Usage: /path/to/emacs -nw -Q -l /path/to/minimal_package_test.el

(toggle-debug-on-error)

(setq package-user-dir (format "%s/elpa--test-lsp/%s" user-emacs-directory emacs-version))

(setq package-selected-packages
      '(;; lsp packages
        lsp-mode     ;; core
        lsp-ui       ;; ui stuff + flycheck support
        lsp-treemacs ;; more ui stuff
        company-lsp  ;; company support
        ccls         ;; ccls support
        helm-lsp     ;; helm support
        lsp-origami  ;; code folding support
        helm-lsp     ;; helm interation
        dap-mode     ;; debugger support
        yasnippet    ;; helpers

        ;; major modes not in core
        dockerfile-mode
        go-mode
        typescript-mode))

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(package-install-selected-packages)

(require 'package)
(defun list-installed-package ()
  (mapcar
   #'car
   (mapcar
    (lambda (p) (cons (package-desc-full-name p) p))
    (delq nil
          (mapcar (lambda (p) (unless (package-built-in-p p) p))
                  (apply #'append (mapcar #'cdr package-alist)))))))

;; ------------------------------------------------------------------

;; lsp configuration begin
(with-eval-after-load 'lsp-mode
  (require 'yasnippet))

(add-hook 'prog-mode-hook 'lsp-deferred)

(setq lsp-log-io t)
(setq-default lsp-ui-sideline-show-hover t)
(require 'lsp-go)
(require 'lsp-html)
;; lsp configuration end

(yas-global-mode)
(ido-mode)


;;; minimal_package_test.el ends here
