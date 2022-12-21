(defvar *db* ())

(defun make-cd (title artist rating ripped?)
  (list :title title :artist artist :rating rating :ripped? ripped?))

(defun add-record (cd)
  (push cd *db*))

(defun show-records ()
    (format t "~{~{~a~7t: ~a~%~}~%~}" *db*))

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (format t "~%")
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
    (if (equal (prompt-read "Ripped? [y/n]") "y") t nil)))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
  (if (not (y-or-n-p "Another? [y/n]: ")) (return))))
