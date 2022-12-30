(defparameter *lower_limit* 1)
(defparameter *upper_limit* 100)

(defun random-in-range (start end)
  (+ start (random (+ 1 (- end start)))))

(defparameter *guess* (random-in-range *lower_limit* *upper_limit*))

(defun guess () *guess*)

(defun guess-again ()
  (setf *guess* (ash (+ *lower_limit* *upper_limit*) -1))
  *guess*)

(defun smaller ()
  (setf *upper_limit* (1- *guess*))
  (guess-again))

(defun bigger ()
  (setf *lower_limit* (1+ *guess*))
  (guess-again))


(defun start-over ()
  (setf *lower_limit* 1)
  (setf *upper_limit* 100)
  (setf *guess* (random-in-range *lower_limit* *upper_limit*)))

