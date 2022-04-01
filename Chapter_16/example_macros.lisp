;;;; This file contains example macros and their expansions.


;;; This macro makes the let command appear nicer, but can only
;;; be used for one variable, unlike the original let.
(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body)) ; ,@ is used in quasi-quoting mode to splice a list.

;; CL-USER> (macroexpand '(let1 foo (+ 2 3)
;;                         (* foo foo)))
;; (LET ((FOO (+ 2 3))) (* FOO FOO))

;;; This macro is really helpful for creating tail recursions.
;;; val is a list. It if can be split function yes is run.
;;; if it can't be split function no is run.
;;; it also returns variables head and tail, i.e. 
(defmacro split (val yes no)
  (let1 g (gensym) ; this is used to avoid variable capturing
    `(let1 ,g ,val
	   (if ,g
	       (let ((head (car ,g)) ; head and tail are accessable outside the macro, 
		     (tail (cdr ,g))); but only if val exists, i.e. is non-empty.
		 ,yes)
	       ,no))))

;; CL-USER> (macroexpand '(split '(2 3)
;;			   (+ x head)
;;			   nil))
;; (LET ((#:G22201 '(2 3)))
;;  (IF #:G22201 (LET ((HEAD (CAR #:G22201)) (TAIL (CDR #:G22201))) (+ X HEAD))
;;   NIL))

;;; This is how to use it:
(defun my-length-1 (lst)
  (labels ((f (lst acc)
	     (split lst
		    (f tail (1+ acc))
		    acc)))
    (f lst 0)))

;; CL-USER> (my-length-1 '(1 2 3 4 5 6 7 8 9))
;; 9

;;; Macro to make tail recursion easier.
(defmacro recurse (vars &body body)
  (let1 p (pairs vars) ; make list of even number vars into key-value pais.
    `(labels ((self ,(mapcar #'car p)
		,@body))
       (self ,@(mapcar #'cdr p)))))

(defun pairs (lst)
  (labels ((f (lst acc)
	     (split lst
		    (if tail
			(f (cdr tail) (cons (cons head (car tail)) acc))
			(reverse acc))
		    (reverse acc))))
    (f lst nil)))

;;; this is how it works.
;; CL-USER> (pairs '(a 1 b 2 c 3))
;; ((A . 1) (B . 2) (C . 3))

(defun liftoff ()
  (recurse (n 10)
    (fresh-line)
    (if (zerop n)
	(princ "Lift-off!")
	(progn (princ n)
	       (self (1- n))))))

;; Here the new recurse-macro is used!
;; CL-USER> (liftoff)
;; 10
;; 9
;; 8
;; 7
;; 6
;; 5
;; 4
;; 3
;; 2
;; 1
;; Lift-off!

(defun my-length-2 (lst)
  (recurse (lst lst
		acc 0) ; it's a bit clunky that we have to name our variables here.
    (split lst
	   (self tail (1+ acc))
	   acc)))

;; Here both recurse macro and split macro is used!
;; CL-USER> (my-length-2 '(1 2 3 4 5 6 7 8 9))
;; 9

;; However, there's already a pre-made macro called reduce, which is much better than our own home made macro!
(defun my-length-3 (lst)
  (reduce (lambda (x i) ; x is accumulator by default, and i is the item in the list.
	    (1+ x))
	  lst
	  :initial-value 0)) ;initial value of the accumulator.

;; This is output
;; CL-USER> (my-length-3 '(1 2 3 4 5 6 7 8 9))
;; 9



