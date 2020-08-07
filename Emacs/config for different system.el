
 ;; ================== Config for different system ==================
 (cond
  ((string-equal system-type "darwin") ; Mac OS X
   (progn
     (message "Mac OS X")))
  ((string-equal system-type "gnu/linux") ; linux
   (progn
     (message "Linux")))
  ((string-equal system-type "windows-nt") ; Microsoft Windows
   (progn
     (message "Microsoft Windows"))))
