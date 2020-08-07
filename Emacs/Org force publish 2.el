https://stackoverflow.com/questions/21258769/using-emacs-org-mode-how-to-publish-the-unchanged-files-in-a-project
---

;; When optional argument FORCE is non-nil, force publishing all
;; files in PROJECT.  With a non-nil optional argument ASYNC,
;; publishing will be done asynchronously, in another process.
(org-publish "notes" t)