;;相当于匿名函数
 (lambda (arguments-list)
   "documentation string"
   body)
;;调用lambda如下: 
 
(funcall (lambda (name)
            (message "Hello, %s!" name)) "Emacser")
            
;;你也可以把 lambda 表达式赋值给一个变量， 后用 funcall 调用
 
 (setq foo (lambda (name)
             (message "Hello, %s!" name)))
 (funcall foo "Emacser")                   ; => "Hello, Emacser!"
