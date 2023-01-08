;; can have a really long number
(expt 143 234)

;;; rational numbers
(/ 4 6) ; 2/3
; with fraction
(/ 4 6.0) ; 0.6666667

;;; form
; a list with symbol as it's first element
(ash 10 1) ; 20

;;; quote
; a special form that return as data without executing it
(quote (oddp 3))
; ' is a shorthand for quote
'(eventp 4)
