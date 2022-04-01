;;;; This game if "Attack of the Robots" from Conrad Barski's
;;;; book "The land of lisp".  This is mainly an exercise in using
;;;; the format-command in lisp, and the various control sequences you can use.

;;; The entire game is just one function named "robots"
(defun robots ()
  (loop named main ; naming the loop
	with directions = '((q . -65)(w . -64)(e . -63)
			    (a .  -1)(s .   0)(d .  +1)
			    (z .  63)(x .  64)(c .  65)) ; locally scoped in loop

	for pos = 544 ; set loop iterator to 544 (halvway into 9th row printed)
	  then (progn (format t "~%qwe/asd/zxc to move, (t)teleport or (l)eave: ")
		      (force-output) ; empties output-buffer
		      (let* ((c (read)) ; read returns symbol, read-line returns string.
			     (d (assoc c directions)))
			(cond (d (+ pos (cdr d)))
			      ((eq 't c) (random 1024)) ; playing field is 64 cols x 16 rows
			      ((eq 'l c) (return-from main 'bye)) ; return from named loop
			      (t pos)))) 
	for monsters = (loop repeat 10 ; 10 robots created, second loop clause loops 2 in iterators in 1 loop
			     collect (random 1024))
	  then (loop for mpos in monsters ; nested loop for monsters.
		     collect (if (> (count mpos monsters) 1) ; if two or more monsters have same pos.
				 mpos ; collect mpos, else...
				 (cdar (sort (loop for (k . d) in directions ; sort lowest cons-cell first, closes to player, then take cdar of that cons-cell = direction we need the robot to move.
						   for new-mpos = (+ mpos d) ; calculate all new monster pos, for each m.
						   collect (cons (+ (abs (- (mod new-mpos 64) ; mod 64
									    (mod pos 64)))    ; gives x-distance
								    (abs (- (ash new-mpos -6) ;  divide by 64
									    (ash pos -6))))   ; gives y-distance
								 new-mpos)) ; cons cell will be (distance to player . new monster position)
					     '< ; sort ascending
					     :key #'car)))) ; key to sort, car of the cons-cell. collectec will be a list of all new monster postitions, unless they crashed into each other.
	when (loop for mpos in monsters ; when-do loop, the two loops above was for-then loops. 
		   always (> (count mpos monsters) 1)) ; when true - run code below, when false - run next clause. Loop always returns true if all robots have crashed.
	  return 'player-wins! ; return if above loop evalutates to true, else...
	do (format t ; set out to terminal
		   "~%|~{~<|~%|~,65:;~A~>~}|" ; justify content, 65 characters wide in total. Loop.
		   (loop for p
			   below 1024 ; loop through all positions.
			 collect (cond ((member p monsters)  ; is there's a robot in that position.
					(cond ((= p pos) (return-from main 'player-loses)) ; is robot position = player position
					      ((> (count p monsters) 1) #\#) ; collect # if robots have crashed
					      (t #\A))) ; else, collect A
				       ((= p pos) ; if there's no robot in that position, but the player is there.
					#\@) ; collect @
				       (t
					#\ )))))) ; else collect empty, (space)
		   
				       
						   
				 
		     
		      
			
		      
