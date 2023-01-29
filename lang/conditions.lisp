; only empty list or nil are falsy

; lenght of a list
(defun my-list-length (list)
  (if list
    (+ 1 (list-length (cdr list)))
    0))
(my-list-length '(1 2 3 4)) ; 4

;;;; if
(if () t nil) ; NIL
(if nil t nil) ; NIL
(if 'anything-else t nil) ; T
; all the expressions of a function are executed
; but in if-statement only one of the things after if is evaluated
; if-statement if a special-form
(if (oddp 5) 'odd (/ 1 0))

;; conditional commands are typically special-forms

;;; progn
; progn can be used to execute multiple commands in a single expression
; only the last is evaluated as the value of the expression
(progn (print "hello") 0)

(defvar *odd-number?* nil)
(if (oddp 5)
  (progn (setf *odd-number?* t) 'odd-number)
  'even-number)

;; lisp has other alternatives that include implicit 'progn'

;;; when
; all enclosed expressions are evaluated when condition is true
(when (oddp 5)
  (setd *odd-number?* t)
  'odd-number)

;;; unless
; all enclosed expressions are evaluated when condition is false
(unless (oddp 4)
  (setf *odd-number?* nil)
  'even-number)

; when and unless can't do anything when condition evaluates to opposite.
;   They just return nil

;;;; cond
; can evaluate several conditions
; can handle more than one branch
; first expression of each parathesized part (branch) contains the condition for making
;   the branch active when true
(defun even-or-odd (n)
 (cond ((oddp n)
        (setf *odd-number?* t)
        'odd-number)
       ((evenp n)
        (setf *odd-number?* nil)
        'even-number)))
(even-or-odd 5)
(even-or-odd 4)

;;; case
; lets you supply values to compare against the value of conditional expression

(defun even-or-odd (n)
 (case (oddp n)
       ((t) (setf *odd-number?* t) 'odd-number)
       (otherwise (setf *odd-number?* nil) 'even-number)))

(even-or-odd 5)
(even-or-odd 4)







