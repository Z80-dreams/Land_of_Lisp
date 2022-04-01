;;;; Writes content to file

(defun writefile (content-to-write)
    (with-open-file (my-stream "testfile.txt" :direction :output)
      (print content-to-write my-stream)))
