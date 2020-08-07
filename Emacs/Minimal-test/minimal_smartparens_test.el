;;; Usage: /path/to/emacs -nw -Q -l /path/to/Minimal_package_test.el
(toggle-debug-on-error)

(setq package-user-dir (format "%s/elpa--test-smartparens/%s" user-emacs-directory emacs-version))
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(defun require-packages (&rest packages)
  (dolist (pkg packages)
    (unless (package-installed-p pkg)
      (package-refresh-contents)
      (package-install pkg))
    (require pkg)))

(require-packages
 'smartparens
 )

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

(add-hook 'after-init-hook
          '(lambda ()
             (switch-to-buffer "*.c")
             (insert "// -*- mode: c -*-\n")
             (insert (format "// Installed packages: %s\n" (list-installed-package)))
             (insert "// Press ' below:\n")
             (c-mode)
             (smartparens-mode)
             (execute-kbd-macro "'")
             ))

(run-hooks 'after-init-hook)
;;; test-smartparens.el ends here
