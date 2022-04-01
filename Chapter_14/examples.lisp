;;;; This code is from "The Land of Lisp" by Conrad Barsky.
;;;; It is meant as an aid in learning Common Lisp.

;;; We want to add two to every element in this list, using imparative
;;; funcitional programming styles.
(defparameter *numbers* '(4 7 2 3))

;;; This is how it's done in imperative "ugly" programming.
(defun imperative-add (lst)
  (format t "The numbers are: ~d~%" lst)
  (loop for n below (length lst)
	do (setf (nth n lst) (+ (nth n lst) 2)))
  (format t "Now the numbers are: ~d~%" lst))

;;; This is how it's done in functional programming, without using
;;; higer order functions.
(defun functional-add-A (lst)
  (when lst
    (cons (+ (car lst) 2) (functional-add-A (cdr lst)))))

;;; This is how it's done in functional programming, using
;;; higher order functions.
(defun functional-add-B (lst)
  (mapcar (lambda (x)
	    (+ x 2))
	  lst))
