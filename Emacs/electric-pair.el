;; electric-pair setting
(electric-pair-mode t)
;; https://www.reddit.com/r/emacs/comments/4xhxfw/how_to_tune_the_behavior_of_eletricpairmode/
(setq electric-pair-inhibit-predicate 'ztlevi/electric-pair-conservative-inhibit)
(defun ztlevi/electric-pair-conservative-inhibit (char)
  (or
   ;; I find it more often preferable not to pair when the
   ;; same char is next.
   (eq char (char-after))
   ;; Don't pair up when we insert the second of "" or of ((.
   ;; (and (eq char (char-before))
   ;;      (eq char (char-before (1- (point)))))
   ;; I also find it often preferable not to pair next to a word.
   (eq (char-syntax (following-char)) ?w)))

(add-hook 'minibuffer-inactive-mode-hook
          #'(lambda() (set (make-local-variable 'semantic-mode) nil)
              (set (make-local-variable 'electric-pair-mode) nil)))
