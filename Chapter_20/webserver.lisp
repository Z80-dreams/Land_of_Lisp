;;;; This is the web server from Chapter 13 of "Land of Lisp" by
;;;; Contrad Barsky. This is not for production, and is an extremely
;;;; unsafe web server, however it is good for learning how to use
;;;; sockets in Lisp, and how the handle strings.

;;; Function to take the http escaped character, and convert to normal character
(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer ; make int from string
	       (coerce (list c1 c2) 'string) ; coerce makes string from list
	       :radix 16 ; our input is hexadecimal
	       :junk-allowed t))) ; return nil instead of raise error
    (if code
	(code-char code) ; code-char returns the ascii char of the int in code.
      default))) ; else pass space (or default variable passed in)

;;; Function to extract parameters from the url passed to it.
(defun decode-param (s)
  (labels ((f (lst)
	      (when lst
		(case (car lst) ; take first character
		      (#\% (cons (http-char (cadr lst) (caddr lst)) ; if %, extract hex-value and make into character
				 (f (cdddr lst))))
		      (#\+ (cons #\Space (f (cdr lst)))) ; if +, make into space
		      (otherwise (cons (car lst) (f (cdr lst))))))))
	  (coerce (f (coerce s 'list)) 'string))) ; sting to list, into f, then to string again.

;;; Function to parse the parameters in the http header, by looking for =
(defun parse-params (s)
  (let* ((i1 (position #\= s))  ; i1 is position of =
	 (i2 (position #\& s))) ; i2 is position of &, if any
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1))) ;from start of string, to position (but not including) i1, make it symbol.
			  (decode-param (subseq s (1+ i1) i2))) ;remaining part of string, until & (or until rest or string, if i2 is nil) is made into string, and http nonsafe characters are made into characters in the list.
		    (and i2 (parse-params (subseq s (1+ i2)))))) ;returns recursive call if i2 evaluates to true.
	  ((equal s "") nil) ; return nil if s is empty string
	  (t s)))) ; if no special characters, just return string.

;;; Function to parse the entire url, and separate out the parameters
(defun parse-url (s)
  (let* ((url (subseq s ; url is between first space from beginning and from end
		      (+ 2 (position #\space s))
		      (position #\space s :from-end t)))
	 (x (position #\? url))) ; if there is an x, extract position
    (if x
	(cons (subseq url 0 x) (parse-params (subseq url (1+ x)))) 
      (cons url '())))) ; make structure, first cons is url, other is parsed params.

;;; Function to get the header info, by searching for :
;;; 
(defun get-header (stream)
  (let* ((s (read-line stream)) ; read stream until lf or cr
	 (h (let ((i (position #\: s))) ; find the : in the line
	     (when i
	       (cons (intern (string-upcase (subseq s 0 i))) ; intern is more secture than read-from-string, as only one symbol is made, not an entire structure.
		    (subseq s (+ i 2))))))) ; make cons of key:value
    (when h
      (cons h (get-header stream))))) ; make alist of all headers

;;; Function to parse the request body
(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header)))) ; if exists
    (when length
      (let ((content (make-string (parse-integer length)))) ; make string of length, length.
	(read-sequence content stream) ; pop off the body of the stream. Header is already popped off. 
	(parse-params content))))) ; parse to replace web safe characters, and to build alist of key:value pairs.

;;; the serve function, this responds to the http request, and sends a response back. The request-handler is the function to call to process request.

(defun serve (request-handler)
  (let ((socket (socket-server 8080)))
    (unwind-protect ; continue to process, even if errors are raised
	(loop (with-open-stream (stream (socket-accept socket))
				(let* ((url (parse-url (read-line stream)))
				       (path (car url))
				       (header (get-header stream))
				       (params (append (cdr url)
						       (get-content-params stream header)))
				       (*standard-output* stream))
				  (funcall request-handler path header params))))
      (socket-server-close socket))))



;;; We also need a request handler to handle the request that the serve function
(defun hello-request-handler (path header params)
  (if (equal path "greeting")
      (let ((name (assoc 'name params)))
	(if (not name)
	    (princ (request-ok (body-wrapper "<form>What is your name?<input name='name' />")))
	    (princ (request-ok (body-wrapper (format nil "Nice to meet you ~a!"(cdr name)))))))
	(princ (request-ok (body-wrapper "Sorry, I don't know that page...")))))


;;; function to return request header
(defun request-ok (body)
  (format nil
"HTTP/1.1 200 OK
Content-Type: text/html; charset=utf-8

~a" body))

;;; function to write the header for dice of doom.
(defun request-ok-dod ()
  (format t
"HTTP/1.1 200 OK
Content-Type: text/html; charset=utf-8

"))

;;; function to wrap html inside head and body tags
(defun body-wrapper (text)
  (format nil
"<!DOCTYPE html>
<html>
<head>
<meta charset='uft-8'>
</head>
<body>~a</body>
</html>" text))



	      
