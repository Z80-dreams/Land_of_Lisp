;;;; Writes an alist to a file

(defparameter *alist* '(a (1 2 3)
			b (4 5 6)
			c (7 8 9)))

(defun writealist ()
  (with-open-file (my-stream "alist-file.txt" :direction :output)
    (print *alist* my-stream)))
