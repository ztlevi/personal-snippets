;;; measure-time.el --- description -*- lexical-binding: t; -*-

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(provide 'measure-time)
;;; measure-time.el ends here
