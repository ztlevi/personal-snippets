(cond (case1 do-when-case1)
 
      (case2 do-when-case2)
      ...
      (t do-when-none-meet))
 
(defun fib (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (fib (- n 1)) (fib (- n 2))))))
 
(fib 10) ; => 55