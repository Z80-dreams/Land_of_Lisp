;;;; This is the game "Dice of Doom" verion 1, similar to Dice Wars, or KDice.
;;;; The Game code is written by Conrad Barsky for the book
;;;; "The land of Lisp", and is used as a teaching aid for learning
;;;; to program in Common Lisp. The comments are added by Maria Aspvik.
;;;; Version 1 contains memoization and tail call recursive optimizations.

(load "lazy.lisp")

;;; Parameters to define the game
(defparameter *num-players* 2)
(defparameter *max-dice* 3) 
(defparameter *board-size* 5) ; 3x3 board. This version can handle 3x3 grids easily in clisp, bigger boards needs more optimization. See ver 2.
(defparameter *ai-level* 4) ; how many levels down in the game tree the ai should search. 
(defparameter *board-hexnum* (* *board-size* *board-size*))

;;; CLEAN FUNCTIONAL ;;;
;;; Code to generate the board-array given a list
(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst))

;;; DIRTY IMPERATIVE
;;; Function to generate random board, uses board-array function.
;;; Since the random function is used, this is not considered functional style.
;;; It breaks the referential transparancy criteria to be considered functional.
(defun gen-board ()
  (board-array (loop for n below *board-hexnum*
		     collect (list (random *num-players*)
				   (1+ (random *max-dice*))))))

;;; CLEAN FUNCTIONAL
;;; Function to replace player numbers with player-letters, using ascii numbers.
(defun player-letter (n)
  (code-char (+ 65 n)))

;;; DIRTY IMPERATIVE
;;; Function to draw the board.
;;; Since it has side effects (it prints to screen) is is considered to be
;;; imperative. It is breaking the critera of no side effects to be considered
;;; functional. 
(defun draw-board (board)
  (loop for y below *board-size* ; loop for rows
	do (progn (fresh-line)
		  (loop repeat (- *board-size* y) ; loop for indent
			do (princ " ")) ; left indent
		  (loop for x below *board-size* ; loop for each column
			for hex = (aref board (+ x (* *board-size* y))) ; row * row-length + column
			do (format t "~a-~a " (player-letter (first hex)) ; Player
				   (second hex)))))) ; dice

;;; CLEAN FUNCTIONAL
;;; This function determines the winner. It can only declare one winner
;;; based on the passed board (referential transparancy), and causes no
;;; side-effects.
(defun winners (board)
  (let* ((tally (loop for hex across board 
		      collect (car hex))) ; who occupies each hex
	 (totals (mapcar (lambda (player)
			   (cons player (count player tally))) ;cons player with number of hexes that player occupies.
			 (remove-duplicates tally))) ; we only need to run mapcar once for each player. 
	 (best (apply #'max (mapcar #'cdr totals)))) ;find maximum of totals.
    (mapcar #'car
	    (remove-if (lambda (x)
			 (not (eq (cdr x) best)))
		       totals)))) ; remove from totals if score is not best, leaivng only the winner and the totals for the winner.

;;; DIRTY IMPERATIVE
;;; Announce winners write to screen, i.e. causes side-effects
(defun announce-winner (board)
  (fresh-line)
  (let ((w (winners board))) ;winner and winner-score
    (if (> (length w) 1) ; if more than 1 winner, i.e. a tie
	(format t "The game is a tie between ~a " (mapcar #'player-letter w)) ;announce all winners if it's a tie.
	(format t "The winner is ~a " (player-letter (car w))))))

;;;; From here on, the code will be broken into three big parts.
;;;; A technique known as "function pipeline" will be used.
;;;; 1. The code that handles the human moves in the game, this part
;;;;    of the code needs to understand the rules of the game, in order
;;;;    to make sure that the human trying to make a legal move, before
;;;;    letting it happen.
;;;; 2. The AI opponent of the game, this AI needs to know the rules
;;;;    of the game, so that it can make legal moves.
;;;; 3. The rules enginge, that defines which rules are legal.

;;;; PART 1 - HUMAN MOVES

;;; DIRTY IMPERATIVE
;;; function for playing against another human. Since it prints to screen it
;;; doesn't fulfill the criteria of no side-effects, and can thus not be
;;; considered functional. As the players always play differently it doesn't
;;; fulfill the requirement for referential transparancy either. 
(defun play-vs-human (tree) ; tree = starting board
  (print-info tree)
  (if (not (lazy-null (caddr tree))) ; double negation, because of lazy evaluation. 
      (play-vs-human (handle-human tree)) ; if any legal moves remain.
      (announce-winner (cadr tree)))) ; if no legal moves remain.

;;; DIRTY IMPERATIVE
;;; Thsi function starts the play vs the computer
(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((lazy-null (caddr tree)) (announce-winner (cadr tree))) ;if no moves possible
	((zerop (car tree)) (play-vs-computer (handle-human tree))) ;if next player is 0
	(t (play-vs-computer (handle-computer tree))))) ;otherwise

;;; DIRTY IMPERATIVE
;;; This function handles printing to screen. This is a side effect and
;;; thus is not functional.
(defun print-info (tree)
  (fresh-line)
  (format t "current player = ~a" (player-letter (car tree)))
  (draw-board (cadr tree))) ; the array for the current board, before any moves

;;; DIRTY IMPERATIVE
;;; This function also prints to screen, and is not functional.
;;; It displays legal moves, and accepts input. It then returns the
;;; new tree, which is a branch of teh old tree. 
(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree))) ; all trees of ((src-dst) player #board moves)
    (labels ((print-moves (moves n) ; because of lazy evaluaton, we have to rewrite the loop to a recursive function.
	       (unless (lazy-null moves)
	       (let* ((move (lazy-car moves)) ; take only (src-dst) cons-cell
		      (action (car move)))
		 (fresh-line)
		 (format t "~a. " n) ; print number n
		 (if action ; if cons-cell for action exists
		     (format t "~a -> ~a" (car action) (cadr action))
		     (princ "end turn")))
	       (print-moves (lazy-cdr moves) (1+ n))))) ; if con-cells doesn't exist.
      (print-moves moves 1))
	     (fresh-line)
	     (cadr (lazy-nth (1- (read)) moves)))) ; return new tree, as a branch of old tree.



;;;; PART 2 - AI MOVES


;;; CLEAN FUNCTIONAL
;;; This is a new rate-positon function that is used with alpha-beta pruning.
(defun ab-rate-position (tree player upper-limit lower-limit)
  (let ((moves (caddr tree)))
    (if (not (lazy-null moves))
	(if (eq (car tree) player)
	    (apply #'max (ab-get-ratings-max tree
					     player
					     upper-limit
					     lower-limit))
	    (apply #'min (ab-get-ratings-min tree
					     player
					     upper-limit
					     lower-limit)))
	(score-board (cadr tree) player))))
					 
	    

;;; CLEAN FUNCTIONAL
;;; This is a new  get-rating function, that prunes the game tree
;;; depending on wheter it is worth to continue searching the branch or not
;;; This algorithm makes the computer play optimally (maximizing outcome)
(defun  ab-get-ratings-max (tree player upper-limit lower-limit)
  (labels ((f (moves lower-limit)
	   (unless (lazy-null moves)
	     (let ((x (ab-rate-position (cadr (lazy-car moves))
					player
					upper-limit
					lower-limit)))
	       (if (>= x upper-limit)
		   (list x) ; not worth searching more, PRUNE!
		   (cons x (f (lazy-cdr moves) (max x lower-limit))))))))
    (f (caddr tree) lower-limit)))


;;; CLEAN FUNCTIONAL
;;; this is a new get-ratings function, that prunes the game tree
;;; dpending on wheter it is worth continue searching the branch or not.
;;; This algorithm assumes that the human plays optimally (assumes that
;;; the human player tries to minimize the computer gains).
(defun ab-get-ratings-min (tree player upper-limit lower-limit)
  (labels ((f (moves upper-limit)
	     (unless (lazy-null moves)
	       (let ((x (ab-rate-position (cadr (lazy-car moves))
					    player
					    upper-limit
					    lower-limit)))
		 (if (<= x lower-limit)
		     (list x) ; not worth searching more, PRUNE!
		     (cons x (f (lazy-cdr moves) (min x lower-limit))))))))
    (f (caddr tree) upper-limit)))
	       

;;; DIRTY IMPERATIVE
;;; This function picks the optimal score for the ai player.
;;; This is imperative because it changes tree
    (defun handle-computer (tree)
      (let ((ratings (ab-get-ratings-max (limit-tree-depth tree *ai-level*)
					 (car tree)
					 most-positive-fixnum
					 most-negative-fixnum))) ; get ratings for next player
	(cadr (lazy-nth (position (apply #'max ratings) ratings) (caddr tree))))) ;get max

;;; CLEAN FUNCTIONAL
;;; This function rates the board at the leaves of the limited tree, based on
;;; how many tiles each player owns, and how treathened each tile is.
;;; This is needed because we cannot evalute each winning position in the full
;;; game tree.
(defun score-board (board player)
  (loop for hex across board
     for pos from 0
     sum (if (eq (car hex) player) ; sum all the points while looping over hex.
	     (if (threatened pos board)
		 1 ; 1 points if player owns hex, but it's threatened.
		 2); 2 points if player owns hex, and it's not threatened.
	     -1))) ; -1 point if player don't own the hex

;;; CLEAN FUNCTIONAL
;;; This function returns true if the board position is threatened, otherwise nil.
(defun threatened (pos board)
  (let* ((hex (aref board pos))
	 (player (car hex))
	 (dice (cadr hex)))
    (loop for n in (neighbors pos)
       do (let* ((nhex (aref board n))
		 (nplayer (car nhex))
		 (ndice (cadr nhex)))
	    (when (and (not (eq player nplayer)) ;if not your own hex
		       (> ndice dice)) ; if more dice than your have
	      (return t))))))
       



;;;; PART 3 - RULES ENGINE

;;; CLEAN FUNCTIONAL
;;; The main function for checking the rules.
;;; This considers all possible moves based on the current board,
;;; Given the current board and current player,
;;; spare dice captured and if the current move is the first move
;;; the player makes, it builds
;;; a tree of legal moves, from the start of the round until every possible
;;; winning move. This function is only called at the beginning of the
;;; move, and the other functions then refer to the game-tree to inspect
;;; if the move is legal. The game-tree will recursivly call itself through
;;; add-passing-move.
    (defun game-tree (board player spare-dice first-move)
      (list player ; each node in tree will be (player #board-array (((move-from move-to)(player #board-array (moves-recursively))((move-from move-to) (player #board-array (moves-recursively)))) 
	    board
	    (add-passing-move board
			      player
			      spare-dice
			      first-move
			      (attacking-moves board player spare-dice))))

;;; Memoization of the game tree. If it sees a sub-tree it has already
;;; calculated once, it doesn't need to to though the entire calculation again.
    (let ((old-game-tree (symbol-function 'game-tree))
	  (previous (make-hash-table :test #'equalp))) ;equalp as key-test instead of default eql, since input is array.
      (defun game-tree (&rest rest) ;any number of arguments, since it's an array.
	(or (gethash rest previous)
	    (setf (gethash rest previous) (apply old-game-tree rest)))))

;;; CLEAN FUNCTIONAL
;;; This function adds passing to the list of legal moves, if the first-move
;;; flag is set to false. It will cons this with the rest of the legal moves.
    (defun add-passing-move (board player spare-dice first-move moves)
      (if first-move
	  moves
	  (lazy-cons (list nil ; name of move, since it passing we name it nil !UPDATE! We now use lazy-cons instead of cons. 
			   (game-tree (add-new-dice board player (1- spare-dice)) ;add dice to board
				      (mod (1+ player) *num-players*) ; next players turn
				      0 ; spare-dice is 0 if we pass to new player
				      t)) ; first-move is true if we pass to new player
		     moves)))

;;; CLEAN FUNCTIONAL
;;; This function caluclates all legal attacking moves (i.e. non-passing moves)
    (defun attacking-moves (board cur-player spare-dice)
      (labels ((player (pos)
		 (car (aref board pos))) ; get player number
	       (dice (pos)
		 (cadr (aref board pos)))) ; get number of dice
	(lazy-mapcan
	 (lambda (src) ; like mapcar, except result is return as flattened list
	   (if (eq (player src) cur-player) ; make sure you only move your own dice
	     (lazy-mapcan
	      (lambda (dst) ; list all legal moves from this pos
		(if (and (not (eq (player dst) cur-player)) ; don't move to a space you already own
			   (> (dice src) (dice dst))) ; only move if your pile is higher than opponents pile.
		  (make-lazy ;make into lazy list instead!
		   (list (list (list src dst) ; move-name from-to
			       (game-tree (board-attack board
							cur-player
							src dst
							(dice src)) ; list subsecurent possible game trees.
					  cur-player
					  (+ spare-dice (dice dst)) ; add opponents dice to your own
					  nil))))
		  (lazy-nil))) ; first move is nil
	      (make-lazy (neighbors src))) ; input list for the second mapcan
	     (lazy-nil)))
	 (make-lazy (loop for n below *board-hexnum*
			  collect n))))) ; input list for the first mapcan.

;;; CLEAN FUNCTIONAL
;;; Function to find all neighbour hexagons from a given tile.
;;; This function is called from e.g. attacking-moves.
    (defun neighbors (pos)
      (let ((up   (- pos *board-size*)) ; we don't care about negative values, we will remove them in the when-collect statement later.
	    (down (+ pos *board-size*))) ; we don't care about pos outside last row, we will remove them in the when-collect statement later.
	(loop for p in (append (list up down) ; 6 legal moves in hexagon grid.
			       (unless (zerop (mod pos *board-size*)) ;unless left edge
				 (list (1- up) (1- pos)))
			       (unless (zerop (mod (1+ pos) *board-size*)) ;unless right edge.
				 (list (1+ pos) (1+ down))))
	      when (and (>= p 0) (< p *board-hexnum*)) ; legal move must be within the board.
		collect p)))

;;; This is memoization of the neighbours function, so that it doesn't have to
;;; calculate everyting over again if the same input is supplied.
    (let ((old-neighbors (symbol-function 'neighbors))
	  (previous (make-hash-table)))
      (defun neighbors (pos)
	(or (gethash pos previous)
	    (setf (gethash pos previous) (funcall old-neighbors pos)))))

;;; CLEAN FUNCTIONAL
;;; function board-attack. This function is called from attacking-moves
;;; it tries to figure out what happens if the source hexagon attacks the
;;; destination haxagon. 
    (defun board-attack (board player src dst dice)
      (board-array (loop for pos from 0; start at 0, increace by 1 each loop.
			 for hex across board ; picking upp hexagon from board
			 collect (cond ((eq pos src) (list player 1)) ;leave one die behind in the src hexagon
				       ((eq pos dst) (list player (1- dice))) ; pleace remaining die in the dst hegaxon
				       (t hex))))) ; else just copy over to new array
    
;;; CLEAN FUNCTIONAL
;;; This function adds a new dice, i.e. create reinforments.
    (defun add-new-dice (board player spare-dice)
      (labels ((f (lst n acc) ; lst is list of all hexagons, n is number of spare dice, acc is accumulator.
		 (cond ((null lst) (reverse acc)) ; if list doens't exist, return nil, i.e. we are at the end of the list. This is a recursive base case. We need to reverse the list though, as the tail recursion puts the last item first in the list. 
		       ((zerop n) (append (reverse acc) lst)) ; if n is 0, this is a recursive base case.
		       (t (let ((cur-player (caar lst))
				(cur-dice (cadar lst)))
			    (if (and (eq cur-player player) (< cur-dice *max-dice*))
				(f (cdr lst)
				   (1- n)
				   (cons (list cur-player (1+ cur-dice)) acc)) ;recursively build up the list, until n is 0, tail recursion.
				(f (cdr lst) n (cons (car lst) acc)))))))) ;if no spare dice, or not your hexagon, just add the car of lst to accumulator and tail recursion. 
	(board-array (f (coerce board 'list) spare-dice ())))) ; make array into list before starting the recursive loop, then make result into array again. () means accumulator is initially empty.
(compile 'add-new-dice) ; Telling clisp to run add-new-dice through the full optimization compiler for tail-recursion.

;;; CLEAN FUNCTIONAL
;;; This function is needed for boards bigger than 4x4, is makes the AI
;;; limit how deep it searches the game tree. This parameter is controlled
;;; by the parameter *ai-level*
(defun limit-tree-depth (tree depth)
  (list (car tree) ; return player
	(cadr tree) ; return board
	(if (zerop depth)
	    (lazy-nil) ; if zero, return no moves, just nil. 
	    (lazy-mapcar (lambda (move) 
			   (list (car move) ; return all moves
				 (limit-tree-depth (cadr move) (1- depth)))) ; all trees that result from each move
			 (caddr tree))))) ; pass in tree to lazy-mapcar




    


