(defun sum-verbose (x y)
  "Sum of two number after printing a message"
  (format t "Summing ~d and ~d.~%" x y)
  (+ x y))
(sum-verbose 10 20)

;; last expression is returned as value of the function
(defun function-value ()
  100)
(function-value) ; 100

;;;; RETURN-FROM
; used to return from a block of code defined with BLOCK special operator
; defun automatically wraps fucntion body in a block with function name
(defun fun-ret-from ()
  (dotimes (i 10
            (when (> i 5) (return-from fun-ret-from t))))
  100)
(fun-ret-from) ; 200


;;;; optional parameters
(defun opt-params-1 (a b &optional c d)
  "c d are optional"
  (list a b c d))
(opt-params-1 1 2 3 4)
(opt-params-1 1 2 3)
(opt-params-1 1 2)

;; default value for optional parameters
(defun opt-params-2 (&optional (a 10) (b 20))
  (list a b))
(opt-params-2 1 2)
(opt-params-2 1)
(opt-params-2)

;; default-value expression can refer to parameters that occur earlier
(defun opt-params-3 (x &optional (y x) (z y)) (list x y z))
(opt-params-3 1 2 3)
(opt-params-3 1)

;; to know if an optional paramenter is supplied, can add another variable after
;; default-value expression. This variable will be T if called supplied the
;; value, otherwise NIL
(defun is-arg-passed  (&optional (x nil x-supplied-p)) x-supplied-p)
(is-arg-passed nil)
(is-arg-passed)


;;;; rest parameter
(defun rest-params (a b &rest rem-params)
  rem-params)
(rest-params 1 2 3 4 5)

;;;; keyword parameters
(defun keyword-params (&key a b c)
  (list a b c))
;; do not need to pass in same order
(keyword-params :c 3 :a 1 :b 2)

;; default-value and -supplied-p
(defun keyword-params-2 (&key (a 10) (b 20 b-supplied-p))
  (list a b b-supplied-p))
(keyword-params-2)

;; parameter alias
(defun keyword-params-3 (&key ((:apple a) 10 a-supplied-p))
  (list a a-supplied-p))
(keyword-params-3 :apple 20)

