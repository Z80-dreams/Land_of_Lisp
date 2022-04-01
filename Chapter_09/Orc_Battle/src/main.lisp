;;;; This is an orc battle text game, from the book "Land of Lisp
;;;; By Conrad Barsky. This game is used as an aid in learning Common Lisp
;;;; Comments added by Maria Aspvik

;;; First, we need to define health, agility and strenth.
;;; When health is 0 the player dies.
;;; Agility determines how many enemies can be attacked in one round.
;;; Strength determines the ferocity of the attacks.
(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)

;;; *monsters* is an array (not list) of all monsters.
;;; *monster-builders* is a list of functions that build different monsters.
;;; *monster-num* controls how many monsters will be created. Change this to adjust difficulty of the game.
(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)

;;; Function to initialise the player
(defun init-player()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

;;; Function to initialise the monsters
(defun init-monsters ()
  (setf *monsters*
	(map 'vector
	     (lambda (x)
	       (funcall (nth (random (length *monster-builders*))
			     *monster-builders*))) ; Call a random function in monster-builders.
	     (make-array *monster-num*))))

;;; Function to check if player is dead
(defun player-dead ()
  (<= *player-health 0))

;;; Function to check if a single monster is dead
(defun monster-dead (m)
  (<= (monster-health m) 0))

;;; Function to check if all monsters are dead
(defun monsters-dead ()
  (every #'monster-dead *monsters*))

;;; The function orc-battle initializes the game, runs the game-loop, and ends the game.
(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. Game Over."))
  (when (monsters-dead)
    (princ "Congratulations! You have vanquished all of your foes.")))

  ;;; The function game-loop checks if player or monsters are dead, handles the rounds and calls itself recursively.
  (defun game-loop ()
    (unless (or (player-dead)
		(monsters-dead))
      (show-player)
      (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15)))) ; k is lexicographically scoped. Minimum 1 is produced.
	(unless (monsters-dead)
	  (show-monsters)
	  (player-attack)))
      (fresh-line)
      (map 'list
	   (lambda (m)
	     (or (monster-dead m)
		 (monster-attack m)))
	   *monsters*)
      (game-loop)))

  ;;; The functions that writes a bunch of player info to the console
  (defun show-player()
    (fresh-line)
    (princ "You are a valiant knight with a health of ")
    (princ *player-health*)
    (princ ", an agility of ")
    (princ *player-agility*)
    (princ ", and a strength of ")
    (princ *player-strength*))

  ;;; Functions that lets the player choose 3 different attack styles, with different outcomes
  (defun player-attack ()
    (fresh-line)
    (princ "Attack style: [s]tab, [d]double swing, [r]roundhouse: ")
    (case (read)
      (s (monster-hit (pick-monster)
		      (+ 2 (randval (ash *player-strength* -1))))) ; pick random between 1 and *player-strength* divided by 2, add 2.
      (d (let ((x (randval (truncate (/ *player-strength* 6))))) ; pick random between 1 and *player-strength* divided by 6.
	   (princ "Your double swing has a strength of ")
	   (princ x)
	   (fresh-line)
	   (monster-hit (pick-monster) x)
	   (unless (monsters-dead)
	     (monster-hit (pick-monster) x))))
      (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 15))))) ; pick random between 1 and *player-strength* divided by 15, do that many times.
		   (unless (monsters-dead)
		     (monster-hit (random-monster) 1)))))) ; Just add 1 damage to each random monster.

  ;;; We need our own random function, that is better suited than the built in one.
  (defun randval (n)
    (1+ (random (max 1 n))))

  ;;; Functon to pick random monster during the chaotic roundhouse attack
  (defun random-monster ()
    (let ((m (aref *monsters* (random (length *monsters*)))))
      (if (monster-dead m)
	  (random-monster)
	  m)))

  ;;; Function to pick a monster during the less chaotic stab and double swing attacks
  (defun pick-monster ()
    (fresh-line)
    (princ "Monster #: ")
    (let ((x (read)))
      (if (not (and (integerp x)(>= x 1)(<= x *monster-num*)))
	  (progn (princ "That is not a valid monster number.")
		 (pick-monster))
	  (let ((m (aref *monsters* (1- x))))
	    (if (monster-dead m)
		(progn (princ "That monster is already dead.")
		       (pick-monster))
		m)))))


;;; Function to print info about all monsters, plus an index the player refers to in pick-monster
(defun show-monsters ()
  (fresh-line)
  (princ "Your foes are: ")
  (let ((x 0))
    (map 'list
	 (lambda (m) ; m is an object, for each monster
	 (fresh-line)
	 (princ "      ")
	 (princ (incf x))
	 (princ ". ")
	 (if (monster-dead m)
	     (princ " ****** DEAD ******")
	     (progn (princ "(Health=")
		    (princ (monster-health m))
		    (princ ") ")
		    (monster-show m))))
	 *monsters*))) ; Array of objects

  ;;; Base-class for monsters. This class will not be used, but will be inherited from. make-monster is the automatically defined constructor function.
(defstruct monster
  (health (randval 10))) ; Health is an attribute (a slot in Lisp-language, setf or defvar is not needed when making defstruct.

(defmethod monster-hit (m x) ; m is the passed in monster-object. Used like "this" or "self" in other languages.
  (decf (monster-health m) x) ; decf is decrease + set
  (if (monster-dead m)
      (progn (fresh-line)
	     (princ "You killed the ")
	     (princ (type-of m)) ; this returns which class the object is based on. (type-of 5) returns "integer", (type-of 'foo) returns "symbol", etc. Very handy!
	     (princ "! "))
      (progn (fresh-line)
	     (princ "Your hit the ")
	     (princ (type-of m))
	     (princ ", knocking off ")
	     (princ x)
	     (princ " health points! "))))

(defmethod monster-show (m) ; Just to be for function overloading. Not to be used directly. 
  (princ "A fierce ")
  (princ (type-of m)))
	
(defmethod monster-attack (m))

;;; Now time to make the orc, this class inherits the monster-class via the :include keyword.
(defstruct (orc (:include monster))
  (club-level (randval 8))) ; club-level is an attribute (a slot in Lisp-language). Setf not needed.
(push #'make-orc *monster-builders*) ; make-org is the automatically defined constructor funciton. Is pushed to the array when the lisp-file is first loaded since it's outside of any fun.

(defmethod monster-show ((m orc)) ; This is called "Type dispatching" in Lisp. If several methods have the same name, the type is checked and the appropriate one is used.
  (princ "A wicked Orc with level ") ; this method is called if m is of type orc. I.e. if (type-of m) returns "orc).
  (princ (orc-club-level m)) ; orc-club-level is slot club-level in class/struct orc for object m.
  (princ " club. "))

;; No monster-hit for orc, just use the normal un-overloaded function.

(defmethod monster-attack ((m orc)) ; type dispatching again
  (let ((x (randval (orc-club-level m)))) ; x is random between 0 and club-level of object orc, for object m.
    (fresh-line)
    (princ "An orc swings his club at you and knocks off ")
    (princ x)
    (princ " of your health points. ")
    (decf *player-health* x)))

;;; Hydra also inherits the monster class, and uses type-dispatching to match the correct methods. The methods are not inside the class/struct, they are separate and just matches the class
(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
  (princ "A malicious hydra with ")
  (princ (monster-health m))
  (princ " heads."))

(defmethod monster-hit ((m hydra) x) ; x is the player attach
  (decf (monster-health m) x)
  (if (monster-dead m)
      (progn (fresh-line)
	     (princ "The corpe of the fully decapitated and decapacitated hydra falls to the floor! "))
      (progn (fresh-line)
	     (princ "You lop off ")
	     (princ x)
	     (princ " of the hyra's heads! "))))

(defmethod monster-attack ((m hydra)) 
  (let ((x (randval (ash (monster-health m) -1)))) ; x random between 0 and monsterhealth divided by two.
    (fresh-line)
    (princ "A hydra attacks you with ")
    (princ x)
    (princ " of its heads! It also grows back one more head! ")
    (incf (monster-health m))
    (decf *player-health*)))

;;; The slime mold attacks the player's agility (the number of attacks the player can do per round)
(defstruct (slime-mold (:include monster))
  (sliminess (randval 5))) ; set to random value when make-slime-mold is called.
(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
  (princ "A slime mold with a sliminess of ")
  (princ (slime-mold-sliminess m))
  (princ " ."))

(defmethod monster-attack ((m slime-mold))
  (let ((x (randval (slime-mold-sliminess m))))
    (fresh-line)
    (princ "A Slime mold wraps around your legs and decreases your agility by ")
    (princ x)
    (princ "! ")
    (decf *player-agility* x) ; decf with a second argument, decreases *player-agility by x.
    (when (zerop (random 2)) ; 1 in 2 chanse that the slime mold squirts and decreases your health. (random returns 0 or 1)
      (princ "It also squirts in your face, taking away a health point! ")
      (decf *player-health*)))) ; decf without a second argument, defaults do decreasing *player-health* by 1.

;;; The Brigand attacks the player's health, the player's strength or the player's agility, whichever is highest. He's a smart foe!
(defstruct (brigand (:include monster)))
(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand))
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
	   (fresh-line)
	   (princ "A brigand hits you with his slingshot, taking off 2 health points! ")
	   (decf *player-health* 2))
	  ((= x *player-agility*)
	   (fresh-line)
	   (princ "A brigand catches your leg with his whip, taking off 2 agility points! ")
	   (decf *player-agility* 2))
	  ((= x *player-strength*)
	   (fresh-line)
	   (princ "A brigand cuts your arm with his whip, taking off 2 strength points! ")
	   (decf *player-strength* 2)))))
  

  
	 
  

  
			  
