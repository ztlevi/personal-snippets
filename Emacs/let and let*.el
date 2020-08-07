 let* 和 let 的使用方式完全相同，唯一的区别是在let*声明中就能使用前面声明的变量，比如:
---

(defun circle-area (radix)
  (let ((pi 3.1415926)
        area)
    (setq area (* pi radix radix))
    (message "    %.2f       %.2f" radix area)))
(circle-area 3)
 
 (defun circle-area (radix)
   (let* ((pi 3.1415926)
(area (* pi radix radix)))
(message "    %.2f       %.2f" radix area)))
