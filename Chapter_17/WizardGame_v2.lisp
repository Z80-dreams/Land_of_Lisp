;;;; Wizard Game.lisp
;;;; I made this project to learn lisp.
;;;; Project taken from the book "The land of lisp" by Conrad Barski.

;; (in-package #:WizardGame)
;; I'll not be using ASDF for this project. 

;;; This is all the nodes where the character can move around in the game.
(defparameter *nodes*
  '((living-room (You are in the living room. A Wizard is snoring loudly on the couch.))
    (garden      (You are in a beautiful garden. There is a well in front of you.))
    (attic       (You are in the attic. There is a gigant welding torch in the corner.))))

;;; Initial location, where the game begins.
(defparameter *location* 'living-room)

;;; This parameter is used to tell the game which moves are permitted within the world.
(defparameter *edges* '((living-room (garden west door)
			             (attic upstairs ladder))
			(garden      (living-room east door))
			(attic       (living-room downstairs ladder))))

;;; This is a list of objects that appear in the game world. This is a simple list.
(defparameter *objects* '(whiskey bucket frog chain))

;;; This list alist describes where the objects are.
(defparameter *object-locations* '((whiskey living-room)
				   (bucket  living-room)
				   (chain   garden)
				   (frog    garden)))

;;; Defines which commands are allowed in the game. To be used together with function
;;; game-eval.
(defparameter *allowed-commands* '(look walk pickup inventory))

;;; This function gives a description of any passed node.
;; This is an exercise in how the assoc function can be used.
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

;;; This function just adds some sugar to the output, for describing the edges.
;; This is an exercise in flipping between data mode and code mode with ` and ,
(defun describe-path (edge)
  `(There is a ,(caddr edge) going ,(cadr edge) from here.))
;;; The following function wraps the previous function, and applies it on multiple edges.
;; The position of the player is location.
;; Takes a function and a list as arguments, then apply the function to every member of the list.
;; # markes the symbol as a function, it's a funciton operator that is necessary when passing
;; function names as variables.
;; Append takes all of the inputs it recives and makes a whole long list of all the inputs.
;; Apply makes sure that all of the output from the describe path goes through the append function.
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;;; This function describes where the objects are.
;;  This is an exercise in the labels function, which is similar to defun,
;;  but scoped locally within the function where it is created.
;;  The location input is the player location. Objects is the list of objects and
;;  obj-locs is the a-list of objects.
(defun objects-at (location objects obj-locs)
  (labels ((at-location-p (objects)
	     (eq (cadr (assoc objects obj-locs)) location)))
	     ; If the input location equals the locaiton given as input.
             ; eq used for symbols, equal for other comparasions.
    (remove-if-not #'at-location-p objects)))
    ; Applies at-locaiton-p to to all memebers in objects, and remove them from the list
    ;(kept in output) if the function returns false for that member.

(defun describe-objects (location objects obj-locs)
  (labels ((describe-single-object (object)
	     `(you see a ,object on the floor.)))
    (apply #'append (mapcar #'describe-single-object (objects-at location objects obj-locs)))))

;;; This is a wrapper function to look around in the room you are, based on your location.
;;  It takes no arguments, hence ()
;;  As usual, append takes three lists in this case, and make it into one list. 
(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths    *location* *edges*)
	  (describe-objects  *location* *objects* *object-locations*)))

;;; This function changes the *location* variable, based on where you are going in the game.
;;  find is used to match the direction variable against the cadr-part of each member.
;; I.e. (cdr (assoc 'living-room *edges)) produces the following list
;; ((GARDEN WEST DOOR) (ATTIC UPSTAIRS LADDER))
;; find is then looking for the direction in the members (WEST) and (UPSTAIRS).
;; :key is a keyword parameter that tells find where to look.
;; If the direction matches, the entire member will be returned from the find function.
;; E.g. if direction is (WEST), then find will return (GARDEN WEST DOOR)
;; We store this list in the variable next, which is scoped locally using the let function. 
(defun walk (direction)
  (let ((next (find direction
		    (cdr (assoc *location* *edges*))
		    :key #'cadr)))
    (if next
	(progn (setf *location* (car next)) ;if part, setf changes *location* to car-part of next.
	       (look)) ;  progn is used to make sure both setf and look are called.
	'(You cannot go that way.)))) ;else part

;;; Function to fick up the object
;; member is used to check if an item is a member in a list.
;; eg. (member bucket (whiskey bucket)) evaluaes to true.
;; Push is used to add a new member to the *object-locations* list, consisting of the symbol for
;; the object, and the symbol 'body. Eg, if the *object-locations list first was the below
;; ((whiskey living-room)(bucket  living-room)(chain   garden)(frog    garden)
;; and whiskey is picked up, the new list will be the below
;; ((whiskey body)(whiskey living-room)(bucket  living-room)(chain   garden)(frog    garden)
;; the list function is just there to make a list of all parameters passed to it.
;; Note, that push places the new member first, while assoc only evalues the first member that match.
;; Hence, when looking in the room again the picked up object appars before the rest of the list,
;; and assoc skipps the already matched things from the *objects* list. 
(defun pickup (object)
  (cond ((member object
		 (objects-at *location* *objects* *object-locations*)) ; end of conditional
		  (push (list object 'body) *object-locations*) ; if true, this evaluates
		  `(You are now carrying the ,object))
	(t '(You cannot get that))))

;;; Function to print out what we have picked up.
(defun inventory()
  (cons 'items-  (objects-at 'body *objects* *object-locations*)))

;;; We don't want the player to insert brackets, quote marks etc,
;;; so we need to make a custom REPL
(defun game-repl()
  (let ((command (game-read)))
    (unless (eq (car command) 'quit) ;if not quit is typed, continue.
      (game-print (game-eval command))
      (game-repl)))) ; loop by recursion

;;; Defining function game-read, game-eval and game-print.
;;  Concatinate has return-type as first argument, then all the strings.
;;  Read-from-string takes a string and makes it into a lisp object.
;;  The list function with parameter 'quote take any symbol and puts a quote in front of it.
;;  E.g. (list 'quote 'hello) becomes 'HELLO
(defun game-read ()
  (let ((command (read-from-string (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
	     (list 'quote x)))
      (cons (car command) (mapcar #'quote-it (cdr command))))))

(defun game-eval (command)
  (if (member (car command) *allowed-commands*) ;if the first symbol in the command is allwed
      (eval command) ; if true
      '(Allowed commands are look walk pickup and inventory. Type quit to quit.))) ; if false

(defun game-print (outstring)
  (princ (coerce (tweak-text (coerce (string-trim "()" ;removes brackets from second argument
						  (prin1-to-string outstring)) ;symbol list to string
				     'list) ;coerce convert string to list of characters
			     t ; 2nd argument to tweak-text, caps=true
			     nil) ;3rd argument to tweak-text, lit=false
		 'string)) ; coerce make list back to string again, and pass to princ
  (fresh-line)) ; prints a new line.

;;; Helper function for game-print
(defun tweak-text (outstring caps lit)
  (when outstring
    (let ((item (car outstring)) ; First word in the string
	  (rest (cdr outstring))) ; The rest of the string
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit))) ; if item is space
	    ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit))) ; if item is ? ! or . then process rest with caps = true.
	    ((eql item #\") (tweak-text rest caps (not lit))) ; if item is ", then process rest with caps = true and lit = not lit.
	    (lit (cons item (tweak-text rest nil lit))) ; if lit = true, process rest with lowercase
	    (caps (cons (char-upcase item) (tweak-text rest nil lit))) ; if caps = true, make item uppercase and process rest with lowercase
	    (t (cons (char-downcase item) (tweak-text rest nil nil))))))) ; else-clause



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Here are some updates, I define macros to easier add new game commands, than add commands to make the game more fun. ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; Macro for adding new game-actions.
;;;; Command is a verb, subj, obj is things you handle, place is where you need to be. E.g. "weld bucket chain".
(defmacro game-action (command subj obj place &body body)
  `(progn (defun ,command (subject object)
	    (if (and (eq *location* ',place) ; where you need to be for this command
		(eq subject ',subj)         ; what stuff you need for this command
		(eq object ',obj)           ; what suff you need for this command
		(have ',subj))              ; what you need in your inventory for this command
	    ,@body
	    '(I cant ,command like that.))) ;else, if ,@body doesn't run.
	  (pushnew ',command *allowed-commands*)))

;;; helper funciton, to check if you have an object in your inventory
(defun have (object)
  (member object (inventory)))

;;; Let's take our new macro for a spin, by defining two new game commands!
;;; First, a command to weld a chain to a bucket.
(defparameter *chain-welded* nil)

(game-action weld chain bucket attic ; will produce (defun weld (chain bucket attic)...
	     (if (and (have 'bucket) (not *chain-welded*)) ;no need to worry about chain and attick, the macro sorts that out!
		 (progn (setf *chain-welded t)
			'(the chain is now securely welded to the bucket.))
		 '(you do not have a bucket.))) ; no need to update *allowed-commands*, the macro do that for us!

;;; Second, a command to dunk the bucket in the well.
(defparameter *bucket-filled* nil)

(game-action dunk bucket well garden
	     (if *chain-welded*
		 (progn (setf *bucket-filled* 't)
			'(The bucket is now full of water))
		 '(The water level is too low to reach.)))

;;; Finally, a game action to splash the water on the snoring wizard in the living-room.
(game-action splash bucket wizard living-room
	     (cond ((not *bucket-filled*) '(The bucket has nothing in it.))
		   ((have 'frog) '(The wizard awakens and sees that you stole his frog.
				   He is so upset that he banishes you to the netherworlds-
				   You lose! The end.))
		   (t '(The wizard awakens from his slumber and greets you warmly.
			He hands you the magic log-carb doughnut- You win! The end.))))

;;; Let's run the game!
(game-repl)



		 
		

