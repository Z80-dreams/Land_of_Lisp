;;;; This file is part of "Land of Lisp" by Conrad Barsky.
;;;; I provides a helpful macros for generating xml. This file
;;;; can then be loaded into files for creating html or xvg.

;;; Macro to print a tag, the inner xml, and the closing tag.
;;; This macro uses the functions and other macros below.
(defmacro tag (name atts &body body) ; body is inner xml
  `(progn (print-tag ',name ;notice the ' in front of name!
		     (list ,@(mapcar (lambda (x)
				       `(cons ',(car x) ,(cdr x))) ; we do all this just to add ' in front of key.
				     (pairs atts)))
		     nil) ; not a closing tag.
	  ,@body ; inner xml
	  (print-tag ',name nil t))) ; closing tag has no attributes!

;; CL-USER> (macroexpand '(tag mytag (color 'blue height (+ 4 5))))
;; (PROGN (PRINT-TAG 'MYTAG (LIST (CONS 'COLOR 'BLUE) (CONS 'HEIGHT (+ 4 5))) NIL)
;;  (PRIN-TAG 'MYTAG NIL T))

;; CL-USER> (tag mytag (color 'blue height (+ 4 5)))
;; <mytag color="BLUE" height="9"></mytag>

;; CL-USER> (tag mytag (color 'blue height (+ 4 5))
;;	     (tag first_inner_tag (font 'nice))
;;	     (tag another_tag (key 'value)))
;; <mytag color="BLUE" height="9">
;;     <first_inner_tag font="NICE">
;;     </first_inner_tag>
;;     <another_tag key="VALUE">
;;     </another_tag>
;; </mytag>

;;; This function is for printing a single tag.
;;; It takes the name of the tag, an attribute list
;;; and a closing tag-predicate as argument.

(defun print-tag (name alst closingp)
  (princ #\<)
  (when closingp
    (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (att)
	  (format t " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
	alst) ; alst must be in format ((k1 . v1) (k2 . v2) (k3 . v3) ...  )
  (princ #\>))

;; CL-USER> (print-tag 'mytag '((k1 . v1) (k2 . v2) (k3 . v3)) nil)
;; <mytag k1="V1" k2="V2" k3="V3">

;;; Function to make a list of format (k1 v1 k2 v2 k3 v3 ... ) into
;;; format ((k1 . v1) (k2 . v2) (k3 . v3) ...  ) i.e. key-value pairs.
(defun pairs (lst)
  (labels ((f (lst acc)
	     (split lst ; uses split-macro
		    (if tail
			(f (cdr tail) (cons (cons head (car tail)) acc))
			(reverse acc))
		    (reverse acc))))
    (f lst nil))) ; tail recursion with accumulator.

;; CL-USER> (pairs '(k1 v1 k2 v2 k3 v3) )
;; ((K1 . V1) (K2 . V2) (K3 . V3)) 

;;; Macro split, used by the pairs fuction
(defmacro split (val yes no)
  (let1 g (gensym) ; to avoid variable capturing of val.
	`(let1 ,g ,val ; put value in unique symbol name.
	       (if ,g
		   (let ((head (car ,g))
			 (tail (cdr ,g)))
		     ,yes) ; if g exists
		   ,no)))) ; if g doesn't exist

;; CL-USER> (macroexpand '(split '(1 2 3) (princ head) (princ "no list!")))
;; (LET ((#:G42196 '(1 2 3)))
;;  (IF #:G42196 (LET ((HEAD (CAR #:G42196)) (TAIL (CDR #:G42196))) (PRINC HEAD))
;;   (PRINC "no list!")))

;; CL-USER> (split '(1 2 3) (progn (princ "head is: ") (princ head) (princ " and tail is: ") (princ tail)) (princ "no list!"))
;; head is: 1 and tail is: (2 3)

;;; let1 macro, used by split macro.
;;; simiplifies the let function is just one symbol is used.
(defmacro let1 (var val &body body)
	  `(let ((,var ,val))
	     ,@body)) ; ,@ splices the list in body ((foo 1 2) (bar 3 4)) becomes (foo 1 2) (bar 3 4)

;; CL-USER> (macroexpand '(let1 foo 123 (if foo (princ "foo is here!"))))
;; (LET ((FOO 123)) (IF FOO (PRINC "foo is here!")))

;; CL-USER> (let1 foo 2 (princ foo))
;; 2



		  
		    
