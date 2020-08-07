;; 条件洛奇的运算和其他语言都是很类似的，使用and, or, not, and 和 or 也同样具有短路性质。很多人喜欢在表达式短时，用and代替when, or代替unless。当然这是一半不关心它们的返回值，而是在于表达式其他句子的副作用。比如or经常用于设置函数的缺省值，而and常用于参数检查。
(defun hello-world (&optional name)
  (or name (setq name "Emacser"))
  (message "Hello, %s" name)) ; => hello-world
(hello-world) ; => "Hello, Emacser"
(hello-world "Ye") ; => "Hello, Ye"
 
(defun square-number-p (n)
  (and (>= n 0) (= (/ n (sqrt n)) (sqrt n))))
(square-number-p -1) ; => nil 
(square-number-p 25) ; => t