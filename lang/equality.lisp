; all are truthy except NIL
; NIL == ()

;;;; EQ
; tests for object identity
; should never use to compare numbers and characters
(eq NIL ()) ; T
(eq 'foo 'foo) ; T

;;;; EQL
; to compare numbers and characters
(eql 123 123) ; T
(eql 12 12.0) ; NIL
(eql #\a #\a) ; T
(eql #\a #\A) ; NIL

;;;; EQUAL
; to compare lists and strings
(equal '(1 2 3) '(1 2 3)) ; T
(equal "hello" "hello") ; T
(equal "Hello" "hello") ; F

;;;; EQUALP
; ignores case for string and characters
; Numbers are equal if they represent the same value
(equalp 12 12.0) ; T
(equalp "Hello" "hello") ; T
(equalp #\a #\A) ; T
