(defvar *db* nil)
(defvar *DB_FILE_NAME* "./cds.db")

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
        (if (not (y-or-n-p "Another? [y/n]: ")) (return)))
  (save-db)
  (format t "~%DB Saved in ./cds.db"))


(defun save-db (&optional (filename *DB_FILE_NAME*))
  (with-open-file (out_file filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out_file))))

(defun load-db (&optional (filename *DB_FILE_NAME*))
  (with-open-file (in_file filename)
    (with-standard-io-syntax
      (setf *db* (read in_file)))))

(defun delete-db ()
  (setf *db* nil)
  (save-db))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun where (&key title artist rating (ripped? nil ripped?-p))
  #'(lambda (cd)
      (and
        (if title (equal (getf cd :title) title) t)
        (if artist (equal (getf cd :artist) title) t)
        (if rating (equal (getf cd :rating) rating) t)
        (if ripped?-p (equal (getf cd :ripped?) ripped?) t))))
