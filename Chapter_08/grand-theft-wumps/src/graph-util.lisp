;;;; The orginal name of this file was alist_to_dotfile, and is from chapter 7. Some comments have been removed to make the file smaller. For mor info, see chapter 7 of The land of Lisp. 

;;;; This program takes a LISP alist, and converts it to a dot-file, which will then
;;;; generate a PNG-file.

;;; First function replaces all non-alphanumeric characters to underscore

(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp)(prin1-to-string exp)))
;;; Substitute if takes three arguments, new item (underscore),
;;; test condition (complement of alphanumeric, i.e. non-alphanumeric)
;;; and the input string.
;;; I.e., exp is passed as input to prin1-to-string, making it a string
;;; rather than a data object.
;;; Alphanumericp returns true is the passed letter is alphanumeric,
;;; otherwise it returns false. We pass this function to complement,
;;; making it work the other way around. #' means this is an argument
;;; sent to a higher order function we are passing in to complement.
;;; If this new function evaluates to true, i.e the character is not alphanumeric
;;; the new character will be inserted instead, i.e. _
;;; living-room becomes living_room, foo! becomes foo_, 24 becomes 24, etc.
;;; all functions ending in p are predicates, i.e. the check the type of
;;; the incoming data, and only return true or false.
;;; Finally #\_ means that _ is of datatype character.

;;; Second function strips the labels of any characters over 30.


(defparameter *max-label-length* 30) ; 30 Characters max.

(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
	(if (> (length s) *max-label-length*)
	    (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
	    s)) ; else-part, also where let-scope ends.
      "")) ; else exp is false, return empty string.

;;; if expression exists, then do the rest.
;;; write-to-string is similar to prin1-to-string above, but can take additonal
;;; keyword argument. :pretty means that linefeed, carriage return, etc are removed
;;; from the resulting string. :pretty is a keyword parameter.
;;; (prin1-to-string "foo!") is equivalent to (write-to-string "foo!" :escape t)
;;; (princ-to-string "bar!") is eq to (write-to-string "bar" :escape nil :readibility :nil)
;;; e.g. (prin1-to-string "foo!") prints "\"foo!\""
;;; e.g. (princ-to-string "bar!") prints "bar!"
;;; this string is assigned to variable s, using the let keyword.
;;; (let ((a 2)(b 5)(c (+ 5 9)))) results in a=2, b=5 and c=14.
;;; Here, the object turned into pretty string is assigned to s, inside the
;;; scope of let. I.e., when let paranthesis ends, the scoping ends.
;;; I.e. (let ((a 2)(b 3))((princ a)))(print a)
;;; The first print works, not the second.
;;; Subseq takes our a section of a string. E.g. (subseq "hello world" 3 8)
;;; return "lo wo".
;;; concatinate takes a keyword to know the appropriate return type.
;;; they can be 'string 'list or 'vector
;;; (concatenate 'string "hi " "yo") returns "hi yo"
;;; (concatenate 'list "hi " "yo" returns (#\h #\i #\  #\y #\o)

;;; Third function writes the actual dot-file

(defun nodes->dot (nodes)
  (mapc (lambda (node)
	  (fresh-line)
	  (princ (dot-name (car node)))
	  (princ "[label=\"") ; Escaping the ", by writing \"
	  (princ (dot-label node))
	  (princ "\"];")) ; Escaping the ", by writing \"
	nodes))

;;; Mapc is like mapcar, except mapcar returns the result of the computation,
;;; and mapc returns the argument. I.e. (mapcar (lambda (x)(* x x))'(1 2 3))
;;; returns (1 4 9), while (mapc (lambda (x)(* x x))'(1 2 3)) returns (1 2 3)
;;; In this case we are writing to a file, so we don't need the result of the computation.
;;; we are writing a new line, followed by the result of the dot-name function, when
;;; the first (car) element of node is passed in. I.e. if node contains
;;; (living-room (you are in the living room...)) we will write "living_room" here.
;;; We then write [label=" followed by the result from passing the node to the
;;; dot-label functin. I.e. the full string, concatinated.


;;; Now, the nodes are done, let's convert the edges in our graph to dot-format

(defun edges->dot (edges)
  (mapc (lambda (node)
	  (mapc (lambda (edge)
		  (fresh-line)
		  (princ (dot-name (car node))) 
		  (princ "->")
		  (princ (dot-name (car edge)))
		  (princ "[label=\"")
		  (princ (dot-label (cdr edge)))
		  (princ "\"];")) ; end of lambda function
		(cdr node))) ; mapc on cdr of node, each one is called edge.
	edges)) ; mapc on edges, each one is called node in the lambda function
	  
;;; edges can be ((living-room (garden west door))(garden (living-room east door))),
;;; so mapc of edges is (living-room (garden west door)), which we store in variable node
;;; car of that, is (living-room).
;;; cdr of node is passed through the lambda function using mapc again. We store this 
;;; in the variable edge. Edge can be (garden west door). car of that is (garden)
;;; We then put the cdr in the dot-label. e.g. (west door)

;;; Now, let's make a wrapper, so that the final syntax for a dot-file is correct.

(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))


;;; Now, let's try to automate!

(defun dot->png (fname thunk)
  (with-open-file (*standard-output*
    (concatenate 'string fname ".dot")
    :direction :output
    :if-exists :supersede)
    (funcall thunk))
  (ext:shell (concatenate 'string "dot -Tpng -O " fname ".dot")))

;;; with-open-file uses stream to read/write to a file. It uses
;;; the open function in the background.
;;; it takes three arguments. stream-pointer, filename and keyword
;;; options. Here we name the stream to *standard-output*, this is
;;; a handle to be used when writing to the file. This name is a special global variable
;;; in Lisp that controls the default location to where output is sent. As such,
;;; anything that the thunk-function tries to print to console, ends up in the file.
;;; After printing is done, the scoping of *standard-output* changes back to its
;;; original output (console). In this regard, with-open-file behaves like let.
;;; We specify the stream to have output direction. If the file
;;; exists, we supersed (replace) it. All keywords symbold in Lisp begin with :, just
;;; as normal symbols begin with '. E.g. 'cigar is a symbol, :cigar is a keyword symbol.
;;; Keyword symbold behave like constants, and cannot be changed. A cigar is just a cigar.
;;; It when takes a function block as and uses that to write to the
;;; file, in our case (funcall thunk), meaning we call the function
;;; thunk that was passed as a parameter to defun dot->png. We could also
;;; write the function directly, but it's easier to pass a thunk, a function handle.
;;; This also makes debugging easier, as the can direct the output of the thunk
;;; to the console for debugging, and once we get it right redirect the
;;; thunk output to dot->png. Remember that princ takes output stream as an argument.
;;; After the dotfile has been written into, we call the shell with the string
;;; "dot -Tpng -O {name of dot-file}

;;; Now, let's write the thunk we spoke about above.

(defun graph->png (fname nodes edges)
  (dot->png fname ; The function that write to a file and calls dot in console
	    (lambda () ; here's the thunk, as an argument to dot->png
	      (graph->dot nodes edges))))




;;; For undirected graphs, we need -- instead of -> in the dot file, and only
;;; one connection whould be shown.

(defun uedges->dot (edges)
  (maplist (lambda (1st)
	     (mapc (lambda (edge)
		     (unless (assoc (car edge)(cdr 1st))
				    (fresh-line)
				    (princ (dot-name (caar 1st)))
				    (princ "--")
				    (princ (dot-name (car edge)))
				    (princ "[label=\"")
				    (princ (dot-label (cdr edge)))
				    (princ "\"];")))
		     (cdar 1st)))
	   edges))

;;; maplist is like mapcar, but receives the entire edges list, not just the current
;;; item of the list. In the next pass it receives the remainder of the list, and so on.
;;; e.g. (mapcar #'print '(a b b) produces "a" "b" and "c"
;;; but (maplist #'print '(a b b) produces "(a b c)", "(b c)" and "(c)"
;;;
;;; For each of the sublists, we run mapc on the cdar of them. 
;;;
;;; E.g.
;;; '((living-room (garden west door)
;;;   (attic upstairs ladder))
;;;   (garden      (living-room east door))
;;;   (attic       (living-room downstairs ladder))))
;;;
;;; cdar of this list is (garden west door),
;;; the cdar of the next maplist is (living-room east door), etc.
;;; each of these are assigned to edge. We run assoc on car of edge,
;;; like (living-room) or (garden) against cdr of 1st, to check if it appears again.
;;; E.g. car of car of edge (garden west door) is garden. Does garden appar in 
;;; 1st? yes, it does appear in cdr or 1st, so we skip this!
;;; Now 1st is this list:
;;;  ((garden      (living-room east door))
;;;   (attic       (living-room downstairs ladder)))
;;; The edge is cdar of 1st, hence (living-room east door)
;;; We check if car of edge exists in 1st. No, living-room does not exist
;;; in cdr of first, by assoc. Hence we print
;;; (garden)--(living-room)[label="(east door)"];

;;; The remaining two functions are the same, just calling uedges->dot instead of
;;; edges->dot, and adding graph{ instead of digraph{ in the beginning. 

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
	    (lambda () ; thunk!
	      (ugraph->dot nodes edges))))




