;;mimic "nzz" behaviou in vim
;;add functions before, after and around function
(defadvice evil-search-next (after advice-for-evil-search-next activate)
  (evil-scroll-line-to-center (line-number-at-pos)))
 
(defadvice evil-search-previous (after advice-for-evil-search-previous activate)
  (evil-scroll-line-to-center (line-number-at-pos)))
 
;;Don't ask me when close emacs with process is running
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))