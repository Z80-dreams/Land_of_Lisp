;;;; Reads an alist from a file

(defparameter *alist* '())

(defun readalist ()
  (with-open-file (my-stream "alist-file.txt" :direction :input)
    (setf *alist* (read my-stream)))
  (print *alist*))

