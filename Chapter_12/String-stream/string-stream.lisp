;;;; String streams are used for debugging, when we want to just send a string
;;;; into a function, instead of sending an actual filestream or socket stream.
;;;; It is also useful for manipulating long strings, or sending strings to
;;;; functions that only accepts streams, e.g. write-to-log. 


(defparameter foo (make-string-output-stream))
(princ "This will go into foo! " foo)
(princ "This also goes into foo! " foo)
(defun viewstring ()
  (get-output-stream-string foo))
