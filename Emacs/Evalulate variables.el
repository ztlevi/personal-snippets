https://emacs.stackexchange.com/questions/7481/how-to-evaluate-the-variables-before-adding-them-to-a-list
---

The general issue is that you need x and y to be evaluated before they get inserted in somelist. The issue with the quoted list (with ' as reader syntax) is that quote is a special form that does not evaluate its argument. According to the docstring:

(quote ARG)

Return the argument, without evaluating it.  (quote x) yields x. Warning: quote does not construct its return value, but just returns the value that was pre-constructed by the Lisp reader...

Hence, you either need to backquote or use a function that evaluates the arguments.

Backquoting allows you to evaluate elements of a backquoted list selectively with the , syntax:

(setq x "x-val" y "y-val" z "z-val" somelist nil)
'(x  y z)                            ; => (x y z)
`(x ,y z)                            ; => (x "y-val" z)
(add-to-list 'somelist `(x y ,z))    ; => ((x y "z-val"))
Alternately, you can use cons (as @tarsius suggests in his answer) or, for an arbitrary number of elements, list:

(add-to-list 'somelist (cons x y))   ; => (("x-val" . "y-val"))
(setq somelist nil)                  ; reset
(add-to-list 'somelist (list x y z)) ; => (("x-val" "y-val" "z-val"))
Which to use depends on what you need to do with the elements.