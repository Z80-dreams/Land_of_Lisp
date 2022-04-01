;;;; Reads content from file
(defun readfile ()
  (with-open-file (my-stream "testfile.txt" :direction :input)
    (princ (read my-stream))))
