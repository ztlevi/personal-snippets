;; my global hungry mode, exceptions for some major mode
(define-global-minor-mode my-global-hungry-delete-mode hungry-delete-mode
  (lambda ()
    (when (not (memq major-mode (list 'python-mode)))
      (hungry-delete-mode))))