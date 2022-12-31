(defvar *db* nil)
(defvar *db_file_name* "./cds.db")

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
  (format t "~%DB Saved in ~a" *db_file_name*))

(defun save-db (&optional (filename *db_file_name*))
  (with-open-file (out_file filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out_file))))

(defun load-db (&optional (filename *db_file_name*))
  (with-open-file (in_file filename)
    (with-standard-io-syntax
      (setf *db* (read in_file)))))

(defun delete-db ()
  (setf *db* nil)
  (save-db))

(defun select-records (selector-fn)
  (remove-if-not selector-fn *db*))

(defun update-records (selector-fn &key title artist rating (ripped? nil ripped?-p))
  (setf *db* (mapcar #'(lambda (row)
                         (when (funcall selector-fn row)
                           (if title (setf (getf row :title) title))
                           (if artist (setf (getf row :artist) title))
                           (if rating (setf (getf row :rating) rating))
                           (if ripped?-p (setf (getf row :ripped?) ripped?)))
                         row) *db*))
  (save-db))

(defun delete-records (selector-fn)
  (setf *db* (remove-if selector-fn *db*))
  (save-db))

(defmacro where (&rest clauses)
  `#'(lambda (cd)
       (and ,@(loop while clauses
                collecting (let ((field (pop clauses))
                                 (value (pop clauses)))
                            `(equal (getf cd ,field) ,value))))))
