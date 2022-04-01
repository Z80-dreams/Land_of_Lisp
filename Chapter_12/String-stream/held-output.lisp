;;;; function "with-output-to-string" captures the specified stream
;;;; in this case *standard-output*, and makes a string of it instead.
;;;; It then returns the string. Below, none of the princ-statements
;;;; are printed to REPL, as they are all intercepted by
;;;; the function with-output-to-string.

(defun maths ()
  (with-output-to-string (*standard-output*)
    (princ "The sum of ")
    (princ 5)
    (princ " and ")
    (princ 2)
    (princ " is ")
    (princ (+ 5 2))))
