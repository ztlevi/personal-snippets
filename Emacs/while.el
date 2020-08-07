(while
    condition
  body)
 
(defun factorial (n)
  (let ((res 1))
    (while (> n 1)
      (setq res (* res n)
            n (- n 1)))
    res))
(factorial 10) ; => 3628800
