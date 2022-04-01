;;;; This is a library with functions that do lazy evaluation.
;;;; They are part of "Land of Lisp" by Conrad Barsky.
;;;; Lazy evaluation in Common Lisp build on Closures, and the
;;;; fact that you can return a function reference, rather than
;;;; a return value, from a let-statement.

;;; first, a macro that causes the body-code passed to it, to be
;;; returned as a reference, under closure. The lambda function
;;; closes over the code send in the body parameter.
(defmacro lazy (&body body)
  (let ((forced (gensym))
	(value  (gensym))) ; First, making sure forced and value don't cause variable capturing.
    `(let ((,forced nil)
	   (,value  nil))
       (lambda ()
	 (unless ,forced
	   (setf ,value (progn ,@body))
	   (setf ,forced t)) ;make sure we only run this function once.
	 ,value))))

;; CL-USER> (macroexpand '(lazy (+ a b)))
;; (LET ((#:G21704 NIL) (#:G21705 NIL))
;;  (LAMBDA NIL
;;   (UNLESS #:G21704 (SETF #:G21705 (PROGN (+ A B))) (SETF #:G21704 T))
;;   #:G21705))

;; the lambda function is returned!
;; CL-USER> (defparameter *add* (lazy (+ a b)))
;; *ADD*
;; CL-USER> (princ *add*)
;; #<FUNCTION LAMBDA NIL
;;   (UNLESS G21804 (SETF G21805 (PROGN (+ A B))) (SETF G21804 T)) G21805>

;;; We need a way to evaluate this funciton. For that we make a simple force-function!
(defun force (lazy-function)
  (funcall lazy-function))

;;; Next, we make some datastructures (cons-cells) and functions we need for working with lists.
(defmacro lazy-cons (a d)
  `(lazy (cons ,a ,d)))

;; Returning a reference to a lambda-function containing cons.
;; CL-USER> (macroexpand '(lazy-cons a d))
;; (LET ((#:G22289 NIL) (#:G22290 NIL))
;;  (LAMBDA NIL
;;   (UNLESS #:G22289 (SETF #:G22290 (PROGN (CONS A D))) (SETF #:G22289 T))
;;   #:G22290))

;; CL-USER> (defparameter *lc* (lazy-cons a b))
;; *LC*
;; CL-USER> (princ *lc*)
;; #<FUNCTION LAMBDA NIL
;;   (UNLESS G22499 (SETF G22500 (PROGN (CONS A B))) (SETF G22499 T)) G22500>

;;; Fucntions to deal with these new lazy cons-cells. They basically just use funcall through force, in
;;; order to call the lambda function, and cause it to evaluate.
(defun lazy-car (x)
  (car (force x)))

(defun lazy-cdr (x)
  (cdr (force x))) ; the cdr part can store another reference to another laxy-cons!

;;; Function that returns nil when called. Can be put in cdr-place of lazy-cons.
(defun lazy-nil ()
  (lazy nil))

;;; Function that checks if function evaluates to nil. Can be used for checking lazy-nil.
(defun lazy-null (x)
  (not (force x)))

;;; Function that converts regular lists into lazy-lists, using the lazy-macro.
(defun make-lazy (lst)
  (lazy (when lst ; remember, lazy returns a reference to a lambda function, which contains the code passed to it.
	  (cons (car lst) (make-lazy (cdr lst))))))

;;; Now we need some functions to do the opposite, make lazy-lists, or parts of them, into regular lists.
(defun take (n lst) ;n - how many elements to convert into regular list.
  (unless (or (zerop n) (lazy-null lst)) ; unless n=0 or the lazy-list evaluates to null. This is the recursive base case.
    (cons (lazy-car lst) (take (1- n) (lazy-cdr lst))))) ; recursive call. 

;;; A function that doesn't use n, it just converts the entire lazy-list. Be careful though, unlike normal lists, a lazy-list
;;; can be infinite! The lambda function in cdr-place just need to be recursive for that!
(defun take-all (lst)
  (unless (lazy-null lst)
    (cons (lazy-car lst) (take-all (lazy-cdr lst)))))

;;; Just to show, this would not work with take-all, as it is an infinite lazy-list, as cdr is infinitely recursive.
(defparameter *integers*
  (labels ((f (n)
	     (lazy-cons n (f (1+ n)))))
    (f 1)))

;;; Now we need our old friend mapcar. This function takes a list and a function, and applies the function
;;; to each element of the list.
(defun lazy-mapcar (fun lst)
  (lazy (unless (lazy-null lst)
	  (cons (funcall fun (lazy-car lst)) ; just cons the results. 
		(lazy-mapcar fun (lazy-cdr lst))))))

;;; We also need mapcan, which is similar but the output list generated is flattened.
(defun lazy-mapcan (fun lst)
  (labels ((f (lst-cur) ; current car of the list.
	     (if (lazy-null lst-cur) ; if the function call evalutes to null, just call mapcan recursively. 
		 (force (lazy-mapcan fun (lazy-cdr lst)))
		 (cons (lazy-car lst-cur) (lazy (f (lazy-cdr lst-cur)))))))
    (lazy (unless (lazy-null lst) ; this will just return a function reference to initiate the recursion.
	    (f (funcall fun (lazy-car lst))))))) ; pass in the result from the function call on car.

;;; A function for finding if a function returns true.
(defun lazy-find-if (fun lst)
  (unless (lazy-null lst)
    (let ((x (lazy-car lst)))
      (if (funcall fun x)
	  x ; if function call on car of lst returns true, return that item.
	  (lazy-find-if fun (lazy-cdr lst))))))

;;; A function to get the nth element from a lazy-list, using lazy-car, lazy-cdr and recursion.
(defun lazy-nth (n lst)
  (if (zerop n)
      (lazy-car lst)
      (lazy-nth (1- n) (lazy-cdr lst))))


	     



