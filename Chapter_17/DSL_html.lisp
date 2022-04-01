;;;; This file if part of the "Land of Lisp" textbook, by Conrad Barsky

;;; First we load some helper macros from the DSL_xml file. 
(load "./DSL_xml.lisp")

;;; Then we make a macro that returns a function that prints and html tag.
(defmacro html (&body body)
  `(tag html ()
     ,@body))

(defmacro head (&body body)
  `(tag head ()
     ,@body))

(defmacro body (&body body)
  `(tag body ()
     ,@body))

;;; Example use
;; CL-USER> (html (progn
;;		   (head 
;;		     (princ "Hello Lisp!"))
;;		   (body
;;		     (princ "<h1>Hello Lisp!</h1>"))))
;; <html>
;;     <head>Hello Lisp!</head>
;;     <body>
;;         <h1>Hello Lisp!</h1>
;;     </body>
;; </html>

