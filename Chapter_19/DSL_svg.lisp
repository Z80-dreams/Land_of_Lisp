;;;; This file is part of the "Land of Lisp" by Conrad Barsky

;;; First we load xml macros
(load "DSL_xml.lisp")

;;; Macro for top level SVG tag
(defmacro svg (width height &body body)
  `(tag svg (width ,width height ,height xmlns "http://www.w3.org/2000/svg" ; for SVG
		   "xmlns:xlink" "http://www.w3.org/1999/xlink") ; For links in SVG
     ,@body))

;;; Function to calculate brightness for RBG values. I.e., make someing brither or daker given a certain color.
(defun brightness (color amount)
  (mapcar (lambda (x)
	    (min 255 (max 0 (+ x amount)))) ; calculate each RGB component separately.
	  color))

;; CL-USER> (brightness '(255 128 128) -100)
;; (155 28 28)

;;; Next is a function to calculate the style-string that goes into the SVG-tags.
;;; this style-string contains information about the fill and the border of the shapes.
;;; It makes the border -100 darker by default, thus we only need to specify one color
(defun svg-style (color)
  (format nil
	  "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}" ; remember, ~{ ~} controls looping within a string.
	  (append color                                 ; 3 values
		  (brightness color -100))))            ; 3 more values, 6 in total.

;; CL-USER> (svg-style '(200 128 128))
;; "fill:rgb(200,128,128);stroke:rgb(100,28,28)"

;;; Next, a function to draw a cirle.
(defun circle (center radius color) ; center is a cons cell (x-cordinate . y-cordinate)
  (tag circle (cx (car center) ; tag is from DSL_xml.lisp
		  cy (cdr center)
		  r radius
		  style (svg-style color))))

;; Example usage
;; CL-USER> (cirle '(50 . 50) 25 '(255 0 0))
;; <circle cx="50" cy="50" r="25" style="fill:rgb(255,0,0);stroke:rgb(155,0,0)"></circle>

;;; function to draw a polygon
(defun polygon (points color)
  (tag polygon (points (format nil
			       "~{~a,~a ~}"
			       (mapcan (lambda (tp)
					 (list (car tp) (cdr tp))) ; flatten the list to fit inthe format-loop.
				       points)) ; key-value pairs for all points
		       style (svg-style color))))

;; CL-USER> (polygon '((-3 9) (-2 4) (-1 1) (0 0) (1 1) (2 4) (3 9)) '(255 0 0))
;; <polygon points="-3,9 -2,4 -1,1 0,0 1,1 2,4 3,9 " style="fill:rgb(255,0,0);stroke:rgb(155,0,0)"></polygon>

;;; To test our polygon funciton, we will make a random graph, first we make a function that rturns a random walk
(defun random-walk (value length)
  (unless (zerop length)
  (cons value
	(random-walk (if (zerop (random 2)) ; 50% change to increase or decrease value by 1
			 (1- value)
			 (1+ value))
		     (1- length))))) ; not tail-call optimized.

;; CL-USER> (random-walk 100 10)
;; (100 101 100 99 100 101 102 101 102 103)

;;; Now let's use the random walk and the polygon funciton to make a graph!
(defun make-graph ()
  (with-open-file (*standard-output* "/home/masp/Cloud/Dropbox/Programming_projects/Land_of_Lisp/Chapter_17/graph.svg"
				     :direction :output
				     :if-exists :supersede)
    (svg 900 500 (loop repeat 10 ; make 10 graphs
		       do (polygon (append '((0 . 200)) ; start in corner x=0 from left, y=200 from top.
					   (loop for x from 1 ; increase x by 1 for each step to the right.
						 for y in (random-walk 100 400)
						 collect (cons x y))
					   '((400 . 200))) ; end in x=400, y=200
				   (loop repeat 3
					 collect (random 256))))))) ; make each graph a random color. 
