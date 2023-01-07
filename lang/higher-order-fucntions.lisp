;; functions are just another kind of object, with a name

;;;; FUNCTION (#')
; to get the function object
(defun foo () 'foo)
(function foo) ; same as #'foo

;;;; FUNCALL
; passes arguments to function object
; use when number of arguments are known
(defun fun-funcall (a b) (list a b))
(funcall #'fun-funcall 1 2)

;;;; APPLY
; when arguments are known at runtime
; takes arguments as a list and applies function object with values in the list
(apply (function +) '(1 2 3 4)) ; 10
; can pass loose arguments as long as last aegument is a list
(apply #'+ 10 '(1 2 3 4)) ; 20


;;;; anonymous funciton
; can be used anywhere where a normal function name can be used
(funcall #'(lambda (a b) (+ a b)) 1 2) ; 3
((lambda (a b) (+ a b)) 1 2) ; 3
