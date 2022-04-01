;;;; This is the game "Dice of Doom" verion 1, similar to Dice Wars, or KDice.
;;;; The Game code is written by Conrad Barsky for the book
;;;; "The land of Lisp", and is used as a teaching aid for learning
;;;; to program in Common Lisp. The comments are added by Maria Aspvik.
;;;; Version 1 contains memoization and tail call recursive optimizations.

(load "lazy.lisp")
(load "DSL_svg.lisp")
(load "webserver.lisp")

;;; Parameters to define the game
(defparameter *num-players* 4)
(defparameter *max-dice* 5) 
(defparameter *board-size* 5) ; 3x3 board. This version can handle 3x3 grids easily in clisp, bigger boards needs more optimization. See ver 2.
(defparameter *ai-level* 2) ; how many levels down in the game tree the ai should search. 
(defparameter *board-hexnum* (* *board-size* *board-size*))

;;; Parameters to define the graphical part of the game
(defparameter *board-width* 900) ; pixels
(defparameter *board-height* 500) ; pixels
(defparameter *board-scale* 64) ; The radius (halv the width) of one hexagon in the game grid.
(defparameter *top-offset* 3) ; leave 3 rows empty on top of the game board, (to make stacked dice visable)
(defparameter *dice-scale* 40) ; a single die is 40 px high/wide
(defparameter *dot-size* 0.05) ; the dots on the dice are 5% of the dice width/height.
(defparameter *die-colors* '((255 63 63) (63 63 255) (63 255 63) (255 63 255))) ; color of the two dice groups for two players.

(defparameter *cur-game-tree* nil) ; this parameter is accessed by the dod-request-handler, to output the current game tree to the web based gui.
(defparameter *from-til* nil) ; this parameter is accessed by the dod-request-handler, to output the current game tree to the web based gui.


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

;;; DIRTY IMPERATIVE
;;; This function prints output, so it's imperative. It rolls the dice, each time an attack move is made.
(defun roll-dice (dice-num)
  (let ((total (loop repeat dice-num
		     sum (1+ (random 6)))))
    (fresh-line)
    (format t "On a ~a dice roll ~a. " dice-num total)
    total))

;;; DIRTY IMPERATIVE
;;; This function rolls the dice piles for both players, during an attack move.
;;; It is imperative because it calls roll-dice, which causes side-effects (printing to output)
(defun roll-against (src-dice dst-dice)
  (> (roll-dice src-dice) (roll-dice dst-dice)))

;;;; From here on, the code will be broken into three big parts.
;;;; A technique known as "function pipeline" will be used.
;;;; 1. The code that handles the human moves in the game, this part
;;;;    of the code needs to understand the rules of the game, in order
;;;;    to make sure that the human trying to make a legal move, before
;;;;    letting it happen.
;;;; 2. The AI opponent of the game, this AI needs to know the rules
;;;;    of the game, so that it can make legal moves.
;;;; 3. The rules enginge, that defines which rules are legal.
;;;; 4. The graphcs part - this part generates SVG and html code
;;;;    for the web based version of the game
;;;; 5. The web server. This part handles the requests and responses
;;;;    from a web client. 

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
	     (pick-change-branch (cadr tree) (lazy-nth (1- (read)) moves)))) ; return new tree, as a branch of old tree.



;;;; PART 2 - AI MOVES

;;; CLEAN FUNCTIONAL
;;; This function recursively rates the best move for the AI-player.
    (defun rate-position (tree player)
      (let ((moves (caddr tree))) ;all possible moves (sub-trees)
	(if (not (lazy-null moves)) ; we need double negation for lazy eval.
	    (apply (if (eq (car tree) player) ;if player can move again
		       #'max ; find best move for player
		       #'min) ; find worst move for opponent
		   (get-ratings tree player))
	    (score-board (cadr tree) player)))) ; use score board instead of the old rating for if the board is a winning board or not. This is because we cannot evalute the entire tree, to when the leaves of the restricted tree is reached, we call score board for those leaves, instead.

;;; CLEAN FUNCTIONAL
;;; We are no longer using alpha-beta pruning, as it is very complex to use together with change nodes
(defun get-ratings (tree player)
  (let ((board (cadr tree)))
    (labels ((dice (pos)
	       (cadr (aref board pos))))
      (take-all (lazy-mapcar
		 (lambda (move)
		   (let ((path (car move)))
		     (if path
			 (let* ((src (car path))
				(dst (cadr path))
				(odds (aref (aref *dice-odds*
						  (1- (dice dst)))
					    (- (dice src) 2))))
			   (+ (* odds (rate-position (cadr move) player))
			      (* (- 1 odds) (rate-position (caddr move)
							   player))))
			 (rate-position (cadr move) player))))
		 (caddr tree))))))



;;; Clean functional
;;; This table returns the odds for winning  when rolling two piles of dice against each other.
;;; Eg. two against one has 84% chance of winning. Row is opponent number of dice, column is player number of dice +1.
;;; Minimum two dice are needed to attack.
(defparameter *dice-odds* #(#(0.84 0.97 1.0  1.0)
			    #(0.44 0.78 0.94 0.99)
			    #(0.15 0.45 0.74 0.91)
			    #(0.04 0.19 0.46 0.72)
			    #(0.01 0.06 0.22 0.46)))




	       

;;; DIRTY IMPERATIVE
;;; This function picks the optimal score for the ai player.
;;; This is imperative because it changes tree.
;;; alpha-beta pruning is not implmented in the version with change-nodes, since it's very complex.
(defun handle-computer (tree)
  (let ((ratings (get-ratings (limit-tree-depth tree *ai-level*) (car tree))))
    (pick-chance-branch
     (cadr tree)
     (lazy-nth (position (apply #'max ratings) ratings) (caddr tree)))))


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
							src
							dst
							(dice src)) ; list subsecurent possible game trees.
					  cur-player
					  (+ spare-dice (dice dst)) ; add opponents dice to your own
					  nil)
			       (game-tree (board-attack-fail board ; we add a second game-tree after each move, for when the dice-rolling fails. 
							     cur-player
							     src
							     dst
							     (dice src)) ; list subsecurent possible game trees.
					  cur-player
					  (+ spare-dice (dice dst)) ; add opponents dice to your own
					  nil))))
		  (lazy-nil))) ; first move is nil
	      (make-lazy (neighbors src))) ; input list for the second mapcan
	     (lazy-nil)))
	 (make-lazy (loop for n below *board-hexnum*
			  collect n))))) ; input list for the first mapcan.

;;; DIRTY IMPERATIVE
;;; This is imperative because it calls roll-against, which prints to screen.
;;; This function picks the right game branch to continue, based on dice rolling.
(defun pick-chance-branch (board move)
  (labels ((dice (pos)
	     (cadr (aref board pos))))
    (let ((path (car move)))
      (if (or (null path) (roll-against (dice (car path)) ;player
					(dice (cadr path)))) ;opponent
	  (cadr move) ; winning game tree
	  (caddr move))))) ;losing game tree

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
;;; This function is like board-attack, but it specifies what happens when a roll fails.
(defun board-attack-fail (board player src dst dice)
  (board-array (loop for pos from 0; start at 0, increace by 1 each loop.
			 for hex across board ; picking upp hexagon from board
		     collect (if (eq pos src)
				 (list player 1) ; if the roll fails, just return a hexagon with 1 dice left in it.
				 hex)))) ; leave other hexagons untouched.

    
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
	(board-array (f (coerce board 'list)
			(largest-cluster-size board player) ())))) ; make array into list before starting the recursive loop, then make result into array again. () means accumulator is initially empty.
(compile 'add-new-dice) ; Telling clisp to run add-new-dice through the full optimization compiler for tail-recursion.

;;; CLEAN FUNCTIONAL
;;; This function is used to calculate number of reinforcement dice.
;;; It takes board, player and position of a hex, and returns connected hexagons.
(defun get-connected (board player pos)
  (labels ((check-pos (pos visited)
	     (if (and (eq (car (aref board pos)) player) ; if the owner owns the board position
		      (not (member pos visited))) ; not already counted
		 (check-neighbors (neighbors pos) (cons pos visited)) ; build up the visited list, one hexagon at the time
		 visited)) ; if already counted or not owned by player, just return the visited list again.
	   (check-neighbors (lst visited) ; the list contains all neighbours, and visited is all already visited hexagons. 
	     (if lst
		 (check-neighbors (cdr lst) (check-pos (car lst) visited)) ; add next hexagon to the visited list. 
		 visited)))
    (check-pos pos nil))) ; set visited to empty.

;;; CLEAN FUNCTIONAL
;;; This function us lists of hexagons from the get-connected function, and detemrines the largest cluster.
;;; This is used for calculating number of reinforcement dice a player gets.
(defun largest-cluster-size (board player)
  (labels ((f (pos visited best)
	     (if (< pos *board-hexnum*) ; do not count outside the board!
		 (if (and (eq (car (aref board pos)) player) 
			  (not (member pos visited))) ; if the player owns the position, and it has not been counted
		     (let* ((cluster (get-connected board player pos)) ; get first cluster
			    (size (length cluster)))
		       (if (> size best) ; is new best score/biggest cluster
			   (f (1+ pos) (append cluster visited) size) ; recursive call with new best score, one hex further in the list.
			   (f (1+ pos) (append cluster visited) best))) ; keep old best.
		     (f (1+ pos) visited best)) ; if pos is already visited, we don't ware calling get-conntected again.
		 best))) ; return best score/biggest cluster
    (f 0 nil 0))) ; start at pos 0, visited is empty and best score is 0.
			    

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
			   (cons (car move)
				 (mapcar (lambda (x)
					   (limit-tree-depth x (1- depth)))
					 (cdr move)))) ; we limit both the winnind tree and losing tree. 
			 (caddr tree))))) ; pass in tree to lazy-mapcar

;;;; PART 4 - SVG AND HTML GENERATION

;;; CLEAN FUNCTIONAL
;;; Function to draw a single die.
(defun draw-die-svg (x y col) ; x- and y-positions, and the color of the die.
  (labels ((calc-pt (pt) ; scales the x and y coordinates of each polygon point to the *dice-scale*
	     (cons (+ x (* *dice-scale* (car pt)))
		   (+ y (* *dice-scale* (cdr pt)))))
	   (f (pol col) ; rescales all the points and creates a polygon tag. 
	     (polygon (mapcar #'calc-pt pol) col)))
    (f '((0 . -1) (-0.6 . -0.75) (0 . -0.5) (0.6 . -0.75)) ; top face of die
       (brightness col 40))
    (f '((0 . -0.5) (-0.6 . -0.75) (-0.6 . 0) (0 . 0.25)) ; left face of die
       col)
    (f '((0 . -0.5) (0.6 . -0.75) (0.6 . 0) (0 . 0.25)) ; right face of die
       (brightness col -40))
    (mapc (lambda (x y) ; make the dots on the die, x y is position of die
	    (polygon (mapcar (lambda (xx yy) ; x and y position of each dot.
			       (calc-pt (cons (+ x (* xx *dot-size*))
					      (+ y (* yy *dot-size*)))))
			     '(-1 -1 1 1) ; x for each corner or dot-polygon
			     '(-1 1 1 -1)); y for each corner or dot-polygon
		     '(255 255 255))) ; color of each dot
	  '(-0.05 0.125 0.3 -0.3 -0.125 0.05 0.2 0.2 0.45 0.45 -0.45 -0.2) ; X pos for each dot
	  '(-0.875 -0.80 -0.725 -0.775 -0.70 -0.625 -0.35 -0.05 -0.45 -0.15 -0.45 -0.05)))) ; y pos for each dot. 

;;; CLEAN FUNCTIONAL
;;; Functional to draw the tile, including the tiless on top of it.
;;; x and y is the y and y logical positions of the hegaxon, i.e. where in the board it is. 
;;; pos is the position in the board-array. Hex is the cons-cell for the hexagon
;;; xx and yy is the pixel positions for each hexagon.
;;; col is the color of the tile (different color for different players).
;;; chosen-tile shows if the tile is chosen or not by the player in the web-gui.
(defun draw-tile-svg (x y pos hex xx yy col chosen-tile)
  (loop for z below 2 ; we draw two tiles, 0 and 1, to give a 3D-effect.
	do (polygon (mapcar (lambda (pt)
			      (cons (+ xx (* *board-scale* (car pt)))
				    (+ yy (* *board-scale*
					     (+ (cdr pt) (* (- 1 z) 0.1)))))) ; move 0.1 up to give a 3D-effect.
			    '((-1 . -0.2) (0 . -0.5) (1 . -0.2) (1 . 0.2) (0 . 0.5) (-1 . 0.2))) ; coordinates for each corder of hexagon.
		    (if (eql pos chosen-tile) ; make chosen tile brighter
			(brightness col 100)
			col)))
  (loop for z below (second hex) ; loop for all dice in the tile.
	do (draw-die-svg (+ xx
			    (* *dice-scale*
			       0.3
			       (if (oddp (+ x y z)) ; make x change a bit for odd/even dice, to give an uneven pile of dice.
				   -0.3
				   0.3)))
			 (- yy (* *dice-scale* z 0.8)) col)))

;;; CLEAN FUNCTIONAL
;;; Function to draw the entire game board.
(defun draw-board-svg (board chosen-tile legal-tiles)
  (loop for y below *board-size*
	do (loop for x below *board-size*
		 for pos = (+ x (* *board-size* y)) ; logical position in hexagon-array
		 for hex = (aref board pos) ; contains a cons-cell with (player . numerDice)
		 for xx = (* *board-scale* (+ (* 2 x) (- *board-size* y))) ; lots of geometry
		 for yy = (* *board-scale* (+ (* 0.7 y) *top-offset*)) ; 0.7, because of the angles in the hexagon.
		 for col = (brightness (nth (first hex) *die-colors*) ; different color for each player.
				       (* -15 (- *board-size* y))) ; darker further back on the board, for 3D effect
		 do (if (member pos legal-tiles)
			(tag g() ; group-tag
			  (tag a ("xlink:href" (make-game-link pos))
			    (draw-tile-svg x y pos hex xx yy col chosen-tile)))
			(draw-tile-svg x y pos hex xx yy col chosen-tile))))) ; if not legal move

;;; CLEAN FUNCITONAL
;;; helper function to generate a link to the clickable hexagon tile.
(defun make-game-link (pos)
  (format nil "/game.html?chosen=~a" pos))

;;;; PART 5 - HTTP REQUEST HANDLER

;;; This function is the main reguest handler, this function is passed as argugment to the serv-function.
;;; The serve-function is already capturing *standard-output*, so we can use princ to write the html.
(defun dod-request-handler (path header params)
  (if (equal path "game.html")
      (progn (request-ok-dod)
	     (format t "<!DOCTYPE html>
<html>
<head>
<meta charset='uft-8'>
</head>
<body>"); These are not part of the code by Conrad Barsky. I wrote this to make a better http header and to wrap the content in proper html.
		      (tag center ()
			(format t "Welcome to DICE OF DOOM!")
			(tag br ())
			(let ((chosen (assoc 'chosen params))) ; extracts the chosen tile from the params sent in the get command.
			  (when (or (not *cur-game-tree*) (not chosen))
			    (setf chosen nil)
			    (web-initialize)) ; generates game-tree
			  (cond ((lazy-null (caddr *cur-game-tree*)) ; if no one can move. 
				 (web-announce-winner (cadr *cur-game-tree*)))
				((zerop (car *cur-game-tree*)) ; human is always player 0
				 (web-handle-human
				  (when chosen
				    (read-from-string (cdr chosen))))) ; get the tile the human wants to move from.
				(t (web-handle-computer))))
			(tag br())
			(draw-dod-page *cur-game-tree* *from-tile*))
		      (format t "</body></html>"))
  (request-ok (body-wrapper (format nil "Sorry... I don't know that page.")))))
		   

;;; Function to initialize a new game
;;; This basically just builds a new game tree and saves it in the 
(defun web-initialize ()
  (setf *from-tile* nil)
  (setf *cur-game-tree* (game-tree (gen-board) 0 0 t)))

;;; Function to announce the winner in the web based version of the game
(defun web-announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
	(format t "The game is a tie between ~a" (mapcar #'player-letter w))
	(format t "The winner is ~a" (player-letter (car w)))))
  (tag a (href "game.html")
       (format t " play again")))

;;; This function gets called when player id is 0.
;;; pos is the selected tile we get from the web response. Player want to move from/to here.
(defun web-handle-human (pos)
  (cond ((not pos) (format t "Please choose a hexagon to move from:"))
	((eq pos 'pass) (setf *cur-game-tree*
			      (cadr (lazy-car (caddr *cur-game-tree*)))) ;update game tree
	 (format t "Your reinforcements have been placed. ")
	 (tag a (href (make-game-link nil))
	       (format t "Continue")))
	((not *from-tile*) (setf *from-tile* pos) ; set from-tile at the first click.
	 (format t "Now choose a detsination:"))
	((eq pos *from-tile*) (setf *from-tile nil)
	 (format t "Move cancelled."))
	(t (setf *cur-game-tree* ; set to-tile on the second click (if from-tile already is set)
		 (cadr (lazy-find-if (lambda (move)
				       (equal (car move)
					      (list *from-tile* pos))) ; if it's a legal move, return it. 
				     (caddr *cur-game-tree*)))) 
	   (setf *from-tile* nil)
	   (format t "You may now ")
	   (tag a (href (make-game-link 'pass)) ; make pass move, if first move is made.
		(format t "pass"))
	   (format t " or make another move:"))))


;;; This function gets called when player id is other than 0, i.e. an AI player.
;;; This is just a wrapper for the old text-based function for handle computer moves.
(defun web-handle-computer ()
  (setf *cur-game-tree* (handle-computer *cur-game-tree*))
  (format t "The computer has moved. ")
  (tag script ()
       (format t "window.setTimeout('window.location=\"game.html?chosen=NIL\"',1000)"))) ; set 5 seconds timeout, in case the computer isn't done thinking.

;;; Next, we need to draw the board. The arguments is the game tree (which includes the board),
;;; and the selected tile that should be highlighted.
(defun draw-dod-page (tree selected-tile)
  (svg *board-width*
       *board-height*
       (draw-board-svg (cadr tree)   ; the board
		       selected-tile ; the tile to highlight
		       (take-all     ; legal tiles (these get a polygon and an a-tag wrapped in a g-tag.
			(if selected-tile
			    (lazy-mapcar
			     (lambda (move)
			       (when (eql (caar move) ; check from-moves = selected tile. 
					  selected-tile)
				 (cadar move))) ; return to-moves
			     (caddr tree)) ; all moves
			    (lazy-mapcar #'caar (caddr tree))))))) ; if no from-tile is selected, all hexes in player-moves are legal.


		 
  




