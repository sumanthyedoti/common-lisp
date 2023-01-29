;;; lists
; lists are held together by cons cells
; () == NIL == '() == 'nil
(eq () NIL) ; T
(eq '() NIL) ; T
(eq () 'NIL) ; T

; list is a chain of cons cells
(equal '(2 3 4) (cons 2 (cons 3 (cons 4 nil))))

;;; cons cell
; made of two connected boxes both of which can point at
; .. another cons cell or data type
;   [ . | . ]

'(1 2 3) ;   [1|.] → [2|.] → [3|.] → nil
; NIL is special symbol that is used to terminate a lisp in lisp

;;; cons
; holds references to objects being linked, typically, second items being list
(cons 1 2) ; (1 . 2)
;; add a new item at fornt of the list
(cons 1 '(2 3 4))
(cons 'chicken 'cat)
(cons 'foo' nil)

;;;; CAR
; to get first element of a lisp
(car '(3 2 4)) ; 3

;;;; CDR
; remaining list without the first element
(cdr '(3 2 4)) ; (2 4)

; skip first 2 elments
(cddr '(1 2 3 4)) ; (3 4)

;;; car-cdr combination funcitons
; supported by language upto 4 levels
;; 2nd element
(cadr '(1 2 3)) ; 2

;; 3rd element
(caddr '(1 2 3 4 5)) ; 3

; tail of 2nd element
(cdadr '( 1 (2 3))) ; (3)

; 2nd element of second list
(cadadr '( 1 (2 3))) ; 3

; 2nd element of first element
(cadar '((1 2 3))) ; 2

