;;;; This game is from "The Land of Lisp", Chapter 10, by Conrad Barsky.
;;;; The game is used to learn Common Lisp. Source code by Contrad Barsky,
;;;; Comments by Maria Aspvik.
;;;; This example is adapted from A.K. Dewdney’s article
;;;; “Simulated evolution: wherein
;;;; bugs learn to hunt bacteria,” in the “Computer Recreations” column of
;;;; Scientific American (May 1989: 138-141).

;;; Define the width and height of of the game. Most of this is stepps,
;;; where little plants grow, and few resources are available for our animals.
;;; the jungle in the middle contains more resources and more plants.

(defparameter *width* 100)
(defparameter *height* 30)
(defparameter *jungle* '(45 10 10 10)) ;begin at x=45, y=10, size 10x10.
(defparameter *plant-energy* 80) ;gives 80 days of energy to animal eating plant
(defparameter *reproduction-energy* 200) ;it takes 200 energy to reproduce

;;; Making hash table to search if plants are available at current x-y.
;;; equal for isomorphic objects, not eql or eq.
;;; A cons-cell of x and y will be used for key.
(defparameter *plants* (make-hash-table :test #'equal))

;;; Grow new plants!
;;; Takes a region defined by left start, top start, width and height,
;;; and adds a plant somewhere within that region, by setting the position
;;; for that in the hash-table to true.
;;; Every day in the simulation, two plants will be grown, one in the
;;; jungle and one in the entire world (inkluding jungle).
;;; because jungle is smaller, it will get a higher concentration of plants.
(defun random-plant (left top width height)
  (let ((pos (cons (+ left (random width)) (+ top (random height)))))
    (setf (gethash pos *plants*) t)))

(defun add-plants ()
  (apply #'random-plant *jungle*)
  (random-plant 0 0 *width* *height*))

;;; Struct for animals
;;; x and y positions. If energy is 0 the animal die. It gains energy
;;; from eating plants. Dir is direction it's facing/walking, genes is a list
;;; of 8 integers 0-7, describing how the animal wants to change it's walking
;;; direction.
;;; Walking directions | genes for changing direction:
;;; 0: north-west | forward
;;; 1: north      | forward-right
;;; 2: north-east | right
;;; 3: east       | back-right
;;; 4: south-east | right
;;; 5: south      | back-left
;;; 6: south-west | left
;;; 7: west       | forward-left
(defstruct animal x y energy dir genes)

(defparameter *animals*
  (list (make-animal :x      (ash *width* -1)
		     :y      (ash *height* -1)
		     :energy 1000
		     :dir    0
		     :genes  (loop repeat 8 ; random genes 1-10
				   collecting (1+ (random 10))))))

;;; Move function is called once per day.
;;; It uses dir of the animal to update x and y position for the animal.
;;; animal-dir, animal-x, animal-y refers to property dir, x and y
;;; of struct animal. "animal" refer to that particular instance of
;;; animal, i.e. one of the animal from *animals*
(defun move (animal)
  (let ((dir (animal-dir animal)) 
	(x (animal-x animal))
	(y (animal-y animal)))
    (setf (animal-x animal) (mod (+ x
				    (cond ((and (>= dir 2) (< dir 5)) 1)
					  ((or (= dir 1) (= dir 5)) 0)
					  (t -1))
				    *width*)
				 *width*))
    (setf (animal-y animal) (mod (+ y
				    (cond ((and (>= dir 0) (< dir 3)) -1)
					  ((and (>= dir 4) (< dir 7)) 1)
					  (t 0))
				    *height*)
				 *height*))
    (decf (animal-energy animal))))

;;; Function for making the animal face another direction each day,
;;; based on the genes.
(defun turn (animal)
  ;; First pick a random value between 0 and sum of all genes.
  (let ((x (random (apply #'+ (animal-genes animal)))))
    (labels ((angle (genes x) ; returns random gene-number.
	       ;; Substract the for first gene from the random number, if less
	       ;; then 0, return 0.
	       (let ((xnu (- x (car genes))))
		 (if (< xnu 0)
		     0
		     ;; else recursive call to remaining genes, return add 1.
		     (1+ (angle (cdr genes) xnu))))))
      (setf (animal-dir animal)
	    (mod (+ (animal-dir animal) (angle (animal-genes animal) x))
		 8))))) ; old dir + random gene, mod 8

;;; Function to eat. We just check the hash table if a plant exists,
;;; and if it does, the animal eats it!
(defun eat (animal)
  (let ((pos (cons (animal-x animal) (animal-y animal))))
    (when (gethash pos *plants*) ;when is like if, without else
      (incf (animal-energy animal) *plant-energy*)
      (remhash pos *plants*))))

;;; Function to reproduce!
(defun reproduce (animal)
  (let ((e (animal-energy animal)))
    (when (>= e *reproduction-energy*) ; if energy>reproduction-energy
      (setf (animal-energy animal) (ash e -1)) ; set energy=energy/2
      (let ((animal-nu (copy-structure animal)) ; shallow copy only!
	    (genes     (copy-list (animal-genes animal))) ; explicit deep copy!
	    (mutation  (random 8)))
	;; Select nth element in list genes. Set to itself, plus random number in range
	;; [0,2], minus 1. Make sure we get at least 1. Push to *animals*-list.
	(setf (nth mutation genes) 
	      (max 1 (+ (nth mutation genes) (random 3) -1)))
	(setf (animal-genes animal-nu) genes)
	(push animal-nu *animals*)))))

;;; Simulating one day in our world
;;; Animals with 0 or less energy are killed, then animals get to
;;; turn to a new direction, move one step, eat if they find platns and
;;; reproduce if they have sufficient energy. Then we add two plants, one i jungle
;;; and one in the entire world. 
(defun update-world ()
  (setf *animals* (remove-if (lambda (animal)
			       (<= (animal-energy animal) 0)) ;kill 0-energy animals
			     *animals*))
  (mapc (lambda (animal) ; performing animal management functions
	  (turn animal)
	  (move animal)
	  (eat animal)
	  (reproduce animal))
	*animals*)
  (add-plants)) ; adding two platns

;;; Function to draw the world.
(defun draw-world ()
  (loop for y
	  below *height*
	do (progn (fresh-line)
		  (princ "|") ; left margin
		  (loop for x
			  below *width*
			do (princ (cond ((some (lambda (animal) ; if true for any animal
						 (and (= (animal-x animal) x) ; loop iter.
						      (= (animal-y animal) y))) ; loop iter.
					       *animals*)
					 #\M) ; M denote animal in the graph.
					((gethash (cons x y) *plants*) #\*) ; * is plant
					(t #\space))))
			   (princ "|")))) ; right margin


;;; Function to control how many days to simulate before drawing world. Also initializes the simulation.
(defun evolution ()
  (draw-world)
  (fresh-line)
  (princ "How many days do you want to simulate? ==> ")
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())
	  (t (let ((x (parse-integer str :junk-allowed t))) ; with junk-allowed set to true, non numeric characters will return 0. If set to false it will raise an error. 
	       (if x
		   (loop for i
			   below x
			   do (update-world)
			   if (zerop (mod i 1000))
			   do (princ #\.)) ; draw a dot to screen every time loop passes 1000.
		   (update-world)) ; make sure to update at least one day, in case non-valid integer character was entered. Can be used to just tap enter to see each new day.
	       (evolution)))))) ; Recursive looping.
		   

	


      








