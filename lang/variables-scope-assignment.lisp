; scope is delimited by biding form

;;;; lexical scope
; binding-form: any construct that introduces a new variable name that's usable
; only within the construct
(defun foo (x)
  (format t "Paramenter: ~a~%" x)
  (let ((x 2))
    (format t "Outer LET: ~a~%" x)
    (let ((x 3))
      (format t "Inner LET: ~a~%" x))
    (format t "Outer LET: ~a~%" x))
  (format t "Paramenter: ~a~%" x))
(foo 1)


;;;; LET*
; initial value form can refer to variabels introduced earlier in the varaibles list
(let* ( (x 10)
        (y (+ x 20)))
  (list x y))

; using nested let
(let ((x 10))
  (let ((y (+ x 20)))
    (list x y)))


;;;; closure
; Bindings created when the flow of control enters the LET form will stick around
; .. for as long as needed, i.e., as long as something holds reference to the
; .. function object returned by the LET form.
(defparameter *fn* (let ((count 0)) #'(lambda () (setf count (1+ count)))))
(funcall *fn*) ; 1
(funcall *fn*) ; 2


;;;; global variables
;; defvar
; defvar assign only if the variable is undefined
; Should use DEFVAR to define variables that will contain data you'd want to
; .. keep even if you made a change to the source code that uses the variable
(defvar *foo* 2
  "variable doc string") ; 2
(defvar *foo* 10) ; 2
(defvar *foo2*) ; unbound
(defvar *foo2* 10) ; 10
(defvar *foo2* 20) ; 10

;; defparameter
; defparameter always assign the initial value
; change takes effect when you recompile and reload the file
(defparameter *bar* 10) ; 10
(defparameter *bar* 20) ; 20

;;;; dynamic binding
; unlike lexical binding, which can be referenced by code only within the
; .. lexical scope of the binding form, a dynamic binding cna be referenced by
; .. any code that's invoked during the executin of the binding form.
; all global varaible are dynamic variables
; to temporarily redefine *standard-output* simply rebint it, all the
; .. following code invoked after it within the binding form (downstream code)
; .. will refer to the new value
; at any given time, the most recent binding shadows all other bindings.
; concepually each new binding for a dynamic variable is pushed onto a stack of
; .. bindings for that variable, and references to the variable always use the
; .. most recent bindng. As binding form return, the bindings they created are
; .. popped off the stack, exposing previous binding
(defvar *x* 10)
(defun foo ()
  (format t "top-level x: ~a~%" *x*)
  (let ((*x* 20))
    (format t "x after rebinding: ~a~%" *x*)
    (fun-after-rebinding))
  ;; as with lexical bindings, assigning a new value affects only the current
  ;; .. bidning
  (format t "top-level x: ~a~%" *x*))
(defun fun-after-rebinding ()
    (format t "x in function called within the bind form: ~a~%" *x*))
(foo)


;;;; constants
; all constants are global
; can not be used as a function parameter or rebound with any other binding form
(defconstant +a-const+ 100 "A consant variable")


;;;; assignment
; SETF macro is a general purpose assignment operator
;; (setf place value)
; can examine the form of the place it is assigning to ans expand into
; .. appropriate lower-level operations to manipulate the place
; when palce is a variable it expands to SETQ, which has access to both lexical
; .. and dynamic bindings
; assigning a new value to a binding has effect only in the binding form

(let ((x 10) (y 20))
  ;; can assign multiple places
  (setf x 1 y 2)
  (format t "x: ~a, y: ~a~%" x y)
  ;; return newly assigned value, so can also nest calls
  (setf x (setf y (random 100)))
  (format t "x: ~a, y: ~a~%" x y))

;; setf can assign any place a value, array, hash table, lists, user-defined
;  .. data structure

;;;; modify macro
; based on the current value
; INCF, DECF
; default amount is 1
(let ((x 10))
  (format t "x: ~a~%" x) ; 10
  (incf x)
  (format t "x: ~a~%" x) ; 11
  (decf x 5)
  (format t "x: ~a~%" x)) ; 6
; modify macros are defined in a way that makes then sage to use with palces
; .. where the place expression must be ecaluated only once

;; ROTATEF
(let ((x 10) (y 20) (z 30))
  (format t "x: ~a, y: ~a~%" x y)
  (rotatef x y)
  (format t "x: ~a, y: ~a~%" x y) ; 20 10
  (setf x 10 y 20)
  ;; can be used any number of arguments
  (rotatef x y z)
  (format t "x: ~a, y: ~a, z: ~a~%" x y z)) ; 20 30 10

;; SHIFTF
; shifts values of arguments to left with last argument provided a value
(let ((x 10) (y 20) (z 30))
  (format t "x: ~a, y: ~a, z: ~a~%" x y z)
  (shiftf x y z 100)
  (format t "x: ~a, y: ~a, z: ~a~%" x y z)) ; 20 30 100


