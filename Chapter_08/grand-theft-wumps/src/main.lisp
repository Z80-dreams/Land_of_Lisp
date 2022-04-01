;;;; This program is presented in chapter 8 of "The Land of Lisp", by Conrad Barsky.
;;;; The code is his, and the comments are added by Maria Aspvik, just as an aid in learning Common Lisp. 

(load "../src/graph-util") ; loads graph-util.lisp in same folder.


(defparameter *congestion-city-edges* nil) ; The list that will eventually hold all the edges in the graph over the city.
(defparameter *congestion-city-nodes* nil) ; The list that will eventually hold all the nodes in the graph over the city.
(defparameter *player-pos* nil) ; Variable to hold the player position
(defparameter *visited-nodes* nil) ; The list that will be used when searching for "islands" when the graph is generated.
(defparameter *node-num* 30) ; Number of nodes (places to visit) in the city.
(defparameter *edge-num* 45) ; Number of edges (roads between nodes) in the city.
(defparameter *worm-num* 3)  ; Number of Gruesome Glowworm Gangs in the city.
(defparameter *cop-odds* 15) ; One in X odds that cops will appear on a road.

;;; Function to start a new game
(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city))

;;; Function to find empty node
(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *congestion-city-nodes*))
	(find-empty-node) ; Recursive call if the cdr-part of the node description has something in it.
	x))) 
  
;;; Function to pick out a random node, among all the nodes
(defun random-node ()
  (1+ (random *node-num*))) ; (1+ x) is same as (+ 1 x), similar to the x++ operator in otherlangues. (random x) returns a number between 0 inclusive and x exclusive.

;;; Function to create two directed edges, to represent one undirected edge. This is needed for graph-util to work.
(defun edge-pair (a b)
  (unless (eql a b) ; eql instead of eq because we are comparing numbers, not symbols. equal would also work, but is not needed.
    (list (cons a b)(cons b a))))

;;; Function to make all the edges randomly between nodes.
(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num* ; loop is REALLY completed in lisp, keyword reapeat is used to set the type of loop.
			collect (edge-pair (random-node)(random-node))))) ; collect is the part of the loop that controls the output from the loop.

;;; Function to find all the other nodes with a direct path to it.
(defun direct-edges (node edge-list) ; node is a cons-cell e.g. (a . b), edge-list is a list of cons-cells e.g. ((a . b)(b . a)(c . d)(d . c)(a . d)(d . a)).
  (remove-if-not (lambda (x)
		   (eql (car x) node)) ; if returning false, it will be removed from the returning list.
		 edge-list))

;;; Functon to get all connected nodes from a starting node, using direct-edges as a helper function.
(defun get-connected (node edge-list) ; node is a cons-cell e.g. (a . b), edge-list is a list of cons-cells e.g. ((a . b)(b . a)(c . d)(d . c)(a . d)(d . a)).
  (let ((visited nil)) ; Store all visited nodes here.
    (labels ((traverse (node) ; labels is like flet, but can be used recursively.
	       (unless (member node visited) ; member returns true if node is in list visited. Unless is like a negative if. It has implicit progn after the test-case.
		 (push node visited)         ; push node onto list visited
		 (mapc (lambda (edge)        
			 (traverse (cdr edge)))
		       (direct-edges node edge-list))))) ; call all connected nodes recursively
	     (traverse node)) ; send the first node to the labels traverse function, the rest will run recursively.
    visited)) ; return the full visited list. This is the "island" of connected nodes.

;;; Function to find all the islands. I.e. running get-connected for all nodes and getting a list containing lists of connected notes, e.g. ((1 2 3 4)(5 6)(8 9 10)) for three "islands"
(defun find-islands (nodes edge-list)
  (let ((islands nil)) ; store all islands here
    (labels ((find-island (nodes)
	       (let* ((connected (get-connected (car nodes) edge-list))
		      ;; let* is like let, but make sure each lexical variable is done evaluating before beginning on the next, i.e. prevents multithreading.
		      (unconnected (set-difference nodes connected))) ; set-difference calculates the difference between two sets, and returns a list.
		 (push connected islands)
		 (when unconnected
		   (find-island unconnected))))) ; Pass in the smaller list again, to get all the connected nodes of the unconnected set.
      (find-island nodes))
    islands)) ; return the list of islands

;;; Function that takes a list of islands (a list of lists) and make a bridge-connection between them through the first node in each sublist.
(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands)(caadr islands)) ; first node in first and second sublist (island) is connected with the help of edge-pair function.
	    (connect-with-bridges (cdr islands)))))   ; append recursively.

;;; Function that wraps the previous functions in searching for islands and connecting them with bridges.
(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list)) ;; Append the new edges to the main edge-list. 

;; Function to generate all the nodes and edges in the city, and make sure they are connected.
(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num* ; Once again, loops are strainge, keyword for sets the loop in for-mode. I is the iterator that is collected.
		      collect i))
	 (edge-list (connect-all-islands nodes (make-edge-list))) 
	 (cops (remove-if-not (lambda (x)
				(zerop (random *cop-odds*))) ; if the random number between 0 inclusive and *cop-odds* exclusive is 0, the edge is not removed from the list.
			      edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

;;; Function to make the edges in normal list of cons-cells into alist format
(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1) ; node1 is a number, we will cons it with all nodes its edges connect to.
	    (cons node1 ; cons node 1, with a list of all other nodes it leads to)
		  (mapcar (lambda (edge) ; Edge is a cons-cell (X . Y), where X is current node, and Y is next node. 
			    (list (cdr edge)))
			  (remove-duplicates (direct-edges node1 edge-list)
					     :test #'equal)))) ; use equal as higher order function to test for equality between cons-cells returned by direct-edges.
	  (remove-duplicates (mapcar #'car edge-list)))) ; take the first number in each cons-cell, remove any duplicates. This should be all nodes. Pass this by mapcar into lambda. 
		      
	     
;;; Function to add cops to the edge-alist, using the normal cons-cells list of cops.
(defun add-cops (edge-alist edges-with-cops) ; edge-alist in format ((X (Y)(Z))(Y (Z))), edges-with-cops in format ((X . Y)(Y . X)(Y . Z)(Z . Y))
  (mapcar (lambda (x) ; x each row in edge-alist
	    (let ((node1 (car x)) ; the key in the edge-alist.
		  (node1-edges (cdr x))) ; all the nodes that connects to node1
	      (cons node1
		    (mapcar (lambda (edge) ; edge is each edge in cdr x, i.e. a number
			      (let ((node2 (car edge))) ; convert from '(X) to 'X
			      (if (intersection (edge-pair node1 node2) ; make a temporary cons-cell to compare with edges-with-cops, use equal function through intersection.
						edges-with-cops
						:test #'equal) ; use equal instead of eql to compare cons-cells.
				  (list node2 'cops) ; create a list that says e.g. '(5 COPS), if edges itersect, otherwise just '(5)
				  edge)))
		    node1-edges)))) ; mapcar to lambda
  edge-alist))

;;; Function to get all neighbouring nodes from the node passed to the function
(defun neighbours (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist)))) ; We mapcar over car to convert from '(X) to 'X

;;; Function to check if node b is a neighbour of node a.
(defun within-one (a b edge-alist)
  (member b (neighbours a edge-alist)))

;;; Same as within-one, but checks if node b is within two jumps from node a
(defun within-two (a b edge-alist)
  (or (within-one a b edge-alist) ; within-one is a subset of within-two
      (some (lambda (x) ; some returns true if any of the lambda calls are true. Similar functions are every, notany, notevery and some.
	      (within-one x b edge-alist))
	    (neighbours a edge-alist))))

;;; Function to make all the city nodes, i.e. add wumpus, blood, worms and such. 
(defun make-city-nodes (edge-alist)
  (let ((wumpus (random-node))
	(glow-worms (loop for i below *worm-num* ; loop in for-mode, from 0 inclusive to *worm-num* exclusive, get a list of random numbers.
			  collect (random-node))))
    (loop for n from 1 to *node-num*
	  collect (append (list n)
			  (cond ((eql n wumpus) '(wumpus!)) ; add '(wumpus) to the node alist
				((within-two n wumpus edge-alist) '(blood!))) ; add '(blood) to the node alist
			  (cond ((member n glow-worms) '(glow-worms!))
				((some (lambda (worm)
					 (within-one n worm edge-alist))
				       glow-worms) ; pass in glow-worms to lambda-funtion
				 '(lights!)))
			  (when (some #'cdr (cdr (assoc n edge-alist))) ; alist is e.g. 
			    ;; e.g. alist ((1 (2) (3 COPS)) (3 (1 COPS)) (2 (1) (4 COPS)) (4 (2 COPS)) (6 (5)) (5 (6) (7)) (7 (5) (8)) (8 (7)) (9 (10)) (10 (9)))
			    ;; (assoc 2 alist) returns (1 (1) (4 COPS)), cdr of that is simply ((1) (4 COPS))
			    ;; (some #'cdr ((1) (4 COPS)) first run (cdr (1)), which returns NIL, then it runs (cdr (4 COPS)) which returns COPS, i.e. non-nil and some return true.
			    '(sirens!))))))

;;; Make a smaller graph, that is a subset of the larger city-wide graph, to show only nodes from the *visited-nodes*-list + 1 away.
(defun known-city-nodes ()
  (mapcar (lambda (node) ; node is all nodes visited + all nodes 1 away from visited nodes
	   (if (member node *visited-nodes*)
	       (let ((n (assoc node *congestion-city-nodes*))) ; n is the full list for that node. We might want to append it!
		 (if (eql node *player-pos*)
		     (append n '(*)) ; put * where player position is. Append to the *congestion-city-nodes*
		     n))
	       (list node '?))) ; if not visited, set '? after node number. 
  (remove-duplicates ; List of all nodes 1 away from visited nodes, remove duplicates, append to visited nodes, and return to mapcar lambda
   (append *visited-nodes* 
	   (mapcan (lambda (node) ; mapcan is like mapcar, but appends all output into one long list. 
		     (mapcar #'car ; run car over all of them, to remove paranthesis, i.e. '(X) becomes 'X
			     (cdr (assoc node *congestion-city-edges*)))) ; take the cdr of the edge assoc, i.e list of nearby nodes
		   *visited-nodes*)))))

;;; Make a smaller grah, that is a subset of the larger city-wide graph, to show only edges connecting to the *visited-nodes* + 1 away.
(defun known-city-edges ()
  (mapcar (lambda (node)
	    (cons node (mapcar (lambda (x)
				 (if (member (car x) *visited-nodes*) ; is x is visited, return x, else return only first part of x (no cops)
				     x ; x can be e.g. (4) if it's node 4, or (4 COPS!) if it's a an edge with cops. 
				     (list (car x))))
			       (cdr (assoc node *congestion-city-edges*))))) ; pass in the cdr part of the assoc alist of edges.
	  *visited-nodes*))

  
;;; Function to draw the city map
(defun draw-city ()
  (ugraph->png "city" *congestion-city-nodes* *congestion-city-edges*)) ; will be saved as city.png

;;; Function to only draw the known city
(defun draw-known-city ()
  (ugraph->png "known-city" (known-city-nodes) (known-city-edges)))

;;; Function for walking, wraps the handle-direction function that is used both for walking and charging against the Wumpus.
(defun walk (pos)
  (handle-direction pos nil))

;;; Function for charging, wraps the handle-direction function that is used both for walking and charging against the Wumpus.
(defun charge (pos)
  (handle-direction pos t))

;;; Function to check if new location exists, and move player there. Wraps the handle-new-place which checks all enemies.
(defun handle-direction (pos charging)
  (let ((edge (assoc pos ; variable edge is the edge to pos, if player is permitted to move there, otherwise nil.
		     (cdr (assoc *player-pos* *congestion-city-edges*))))) ; Get all possible directions player can move to from *player-pos*
    (cond ((eql *player-pos* pos)
	   (format t "~&You are already there!"))
	  (edge (progn (handle-new-place edge pos charging)
		       (format t  "~&Updating graph, please reload browser.")))
	  (t (format t "~&That location does not exist!")))))
  

;;; Function that checks for enimies, etc.
(defun handle-new-place (edge pos charging)
  (let* ((node (assoc pos *congestion-city-nodes*)) ; Get the full node description, including enimes.
	 (has-worm (and (member 'glow-worms! node)
			(not (member pos *visited-nodes*))))) ; Glow-worms only attack once!
    (pushnew pos *visited-nodes*) ; Pushnew only push if member doesn't already exist in set.
    (setf *player-pos* pos)
    (draw-known-city)
    (cond ((member 'cops edge) (princ "You ran into the cops! GAME OVER..."))
	  ((member 'wumpus! edge) (if charging
				(princ "You found the Wumpus! WIN...")
				(princ "You ran into the Wumpus! GAME OVER...")))
	  (charging (princ "You wasted you last bullet! GAME OVER..."))
	  (has-worm (let ((new-pos (random-node)))
		      (princ "You ran into a Glow Worm Gang! You're now at ")
		      (princ new-pos)
		      (handle-new-place nil new-pos nil)))))) ; no charging and no edge when Glow Worm Gang lifts you away.
	  





				 
			  
			
