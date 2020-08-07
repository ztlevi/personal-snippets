;; ztlevi-misc package
    ;; helm-github-stars
    ;; helm

(defun ztlevi-misc/post-init-helm ()
  (with-eval-after-load 'helm
    (progn
      ;; limit max number of matches displayed for speed
      (setq helm-candidate-number-limit 100)
      ;; ignore boring files like .o and .a
      (setq helm-ff-skip-boring-files t)
      ;; replace locate with spotlight on Mac
      (setq helm-locate-command "mdfind -name %s %s")
      (push "\\.emlx$" helm-boring-file-regexp-list))))

(defun ztlevi-misc/init-helm-github-stars ()
  (use-package helm-github-stars
    :commands (helm-github-stars)
    :init
    (setq helm-github-stars-username "ztlevi")))


;; custom.el
 '(helm-buffer-max-length 56)
 '(helm-move-to-line-cycle-in-source t)
