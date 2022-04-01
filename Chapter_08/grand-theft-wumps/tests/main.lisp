(load "../src/main")
(format t "Testing of function make-edge-list... ~%~%")
(princ (make-edge-list))

(format t "~%~%Testing get-connected function with four nodes...~%~%")
(princ (get-connected '1 (list (cons 1  2)(cons 2 1)(cons 1 3)(cons 3 1)(cons 2 4)(cons 4 2))))

(format t "~%~%Testing find-islands function with three islands...~%~%")
(princ (find-islands (list 1  2 3 4 5 6 7 8 9 10) ; nodes
	      (list (cons 1  2)(cons 2 1)(cons 1 3)(cons 3 1)(cons 2 4)(cons 4 2); island A
		(cons 5 6)(cons 6 5)(cons 5 7)(cons 7 5)(cons 7 8)(cons 8 7) ; island B
		    (cons 9 10)(cons 10 9)))) ; island C

(format t "~%~%Testing connect-all-islands function with three islands...~%~%")
(let* ((before (list (cons 1  2)(cons 2 1)(cons 1 3)(cons 3 1)(cons 2 4)(cons 4 2); island A
		(cons 5 6)(cons 6 5)(cons 5 7)(cons 7 5)(cons 7 8)(cons 8 7) ; island B
		(cons 9 10)(cons 10 9)))
       (after (connect-all-islands (list 1  2 3 4 5 6 7 8 9 10) ; nodes
	      (list (cons 1  2)(cons 2 1)(cons 1 3)(cons 3 1)(cons 2 4)(cons 4 2); island A
		(cons 5 6)(cons 6 5)(cons 5 7)(cons 7 5)(cons 7 8)(cons 8 7) ; island B
		(cons 9 10)(cons 10 9))))
       (diff (set-difference after before :test #'equal)))
  (format t "~%Before:~%")
  (princ before)
  (format t "~%After:~%")
  (princ after)
  (format t "~%Diff:~%")
  (princ diff))

(format t "~%~%Testing edges-to-alist function with one island...~%~%")
(let* ((before '((1 . 2) (2 . 1) (1 . 3) (3 . 1) (2 . 4) (4 . 2) (5 . 6) (6 . 5) (5 . 7) (7 . 5) (7 . 8) (8 . 7) (9 . 10) (10 . 9)))
       (after (edges-to-alist before)))
  (format t "~%Before:~%")
  (princ before)
  (format t "~%After:~%")
  (princ after))

(format t "~%~%Testing add-cops function, do add cops between 1 and 3, and between 2 and 4...~%~%")
(let ((edge-alist '((1 (2) (3)) (3 (1)) (2 (1) (4)) (4 (2)) (6 (5)) (5 (6) (7)) (7 (5) (8)) (8 (7)) (9 (10)) (10 (9))))
      (cops-edges '((3 . 1)(2 . 4))))
  (princ (add-cops edge-alist cops-edges)))


(format t "~%~%Testing make-city-edges function...~%~%")
(princ (make-city-edges))

(format t "~%~%Testing within-two function function. Starting from node 1, node 2, 5 are one away, and node 4, 6 and 7 are two away...~%~%")
(let ((edge-alist '((1 (2) (5)) (3 (1)) (2 (1) (4)) (4 (2)) (6 (5)) (5 (6) (7)) (7 (5) (8)) (8 (7)) (9 (10)) (10 (9))))
      (test-nodes '(1 2 3 4 5 6 7 8 9 10))
      (start-node '1))
  (mapcar (lambda (x)
	    (format t "Starging from node ~D. " start-node)
	    (format t "Is node ~D two jumps away? " x)
	    (cond ((within-two start-node x edge-alist)(format t "YES!~%"))
		  (t (format t "NO!~%"))))
	  test-nodes))

(format t "~%~%Testing make-city-nodes, using the edge-alist. If COPS exists in alist, then Sirens! should exist in corresponding node-list...~%~%")
(let ((edge-alist (make-city-edges)))
  (format t "Input alist is:~%")
  (princ edge-alist)
  (format t "~%~%")
  (format t "Out node-list is:~%")
  (princ (make-city-nodes edge-alist)))


(format t "~%~%Now, trying to generate png file, look in the folder~%~%")
(new-game)
		  
  



  








