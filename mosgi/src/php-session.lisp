#|
Author:Simon Koch <s9sikoch@stud.uni-saarland.de>
This file contains code to parse serialized php-sessions
|#
(in-package :de.uni-saarland.syssec.mosgi.php-session)


(define-condition php-text-serialized-session-parsing-condition (simple-condition)
  ())


(defclass php-session ()
  ((session-id
    :initarg :session-id
    :initform (error "parameter session-id has to be provided")
    :reader session-id)
   (elements 
    :initarg :elements
    :reader elements)))


(defun make-php-session (session-file-stream session-id)
  (make-instance 'php-session 
		 :elements (parse-string-serialized-session session-file-stream)
		 :session-id session-id))


(defun create-empty-php-session (session-id)
  (make-instance 'php-session
		 :elements nil
		 :session-id session-id))


(defun extract-session-id (filename &key (full-path-p T))
  (error "NYI"))


(defmethod add-element ((session php-session) element-name element-content)
  (setf (gethash element-name (slot-value session 'elements))
	element-content))


(defun extract-size (char-list)
  (when (not (char= (first char-list) #\:))
    (error 'php-text-serialized-session-parsing-condition
	   :format-control "php serialized session bad format as no leading : for size extraction was found in ~a"
	   :format-arguments (list char-list)))
  (values (parse-integer (coerce (subseq (cdr char-list) 0 (position #\: (cdr char-list) :test #'char=)) 'string)) ;this is horrible in type safety (but convinent)
	  (subseq (cdr char-list) (position #\: (cdr char-list) :test #'char=))))


(defun array-elements->element-table (array-elements)
  (when (not (= (mod (length array-elements) 2) 0))
    (error 'php-text-serialized-session-parsing-condition
	   :format-control "uneven content of array - expected associative array?"))
  (do ((table (make-hash-table :test 'equalp))
       (remaining array-elements (cddr array-elements)))
      ((not remaining) table)
    (setf (gethash (cdar remaining) table)
	  (cadr remaining))))


(defun parse-array (char-list)
  (multiple-value-bind (result rem-char-list)
      (extract-size char-list)
    (when (not (char= (car rem-char-list) #\:))
      (error 'php-text-serialized-session-parsing-condition
	     :format-control "bad format detected - expected : but encountered ~a"
	     :format-arguments (list (car rem-char-list))))
     (when (not (char= (cadr rem-char-list) #\{))
      (error 'php-text-serialized-session-parsing-condition
	     :format-control "bad format detected - expected { but encountered ~a"
	     :format-arguments (list (car rem-char-list))))
    (do ((i 0 (+ i 1))
	 (rem-char-list (cddr rem-char-list))
	 (array-elements nil))
	((= i (* 2 result)) (if (char= (car rem-char-list) #\})
				(values (array-elements->element-table (reverse array-elements))
					rem-char-list) ;due to the mindfuck of the syntax need to return } for multiple array nesting
				(error 'php-text-serialized-session-parsing-condition
				       :format-control "bad format detected - expected } but encountered ~a"
				       :format-arguments (list (car rem-char-list)))))
      (multiple-value-bind (result buff-char-list)
	  (extract-element-content rem-char-list)
	(setf rem-char-list buff-char-list)
	(push result array-elements)))))


(defun parse-string (char-list)
  (multiple-value-bind (result rem-char-list)
      (extract-size char-list)
    (when (not (char= (first rem-char-list) #\:))
      (error 'php-text-serialized-session-parsing-condition
	     :format-control "bad format detected - expected : but encountered ~a"
	     :format-arguments (list (car rem-char-list))))
    (let ((string-content (subseq (cdr rem-char-list) 0 (+ result 2)))) ;probably not the type-safest way, eh?
      (if (and (char= (first string-content) #\")
	       (char= (car (last string-content)) #\"))
	  (values (coerce (subseq string-content 1 (- (length string-content) 1)) 'string)
		  (subseq (cdr rem-char-list) (+ result 2)))
	  (error 'php-text-serialized-session-parsing-condition
		 :format-control "bad lenght parameter ~a encountered when parsing session string ~a"
		 :format-arguments (list result char-list))))))


(defun extract-element-content (char-list)
  (when (not char-list)
    (return-from extract-element-content nil))
  (multiple-value-bind (result rem-char-list)
      (ecase (car char-list)
	(#\a 
	 (multiple-value-bind (result rem-char-list)
	     (parse-array (cdr char-list))
	   (values (cons :ARRAY result) rem-char-list)))
	(#\s 
	 (multiple-value-bind (result rem-char-list)
	     (parse-string (cdr char-list))
	   (values (cons :STRING result) rem-char-list))) 
	(#\N
	 (values (cons :EMPTY nil)
		 (cdr char-list))))
    (when (and (car rem-char-list)
	       (not (char= (car rem-char-list) #\;))
	       (not (char= (car rem-char-list) #\})))
      (error 'php-text-serialized-session-parsing-condition
	     :format-control "bad format detected - expected ; or eof but encountered ~a"
	     :format-arguments (list (car rem-char-list))))
    (values result (cdr rem-char-list))))
     

(defun extract-element-name (char-list)
  (when (not (find #\| char-list :test #'char=))
    (error 'php-text-serialized-session-parsing-condition
	   :format-control "malformed serialized line - no I encountered ~a"
	   :format-arguments (list (coerce char-list 'string))))
  (values (coerce (subseq char-list 0 (position #\| char-list :test #'char=)) 'string)
	  (subseq char-list (+ (position #\| char-list :test #'char=) 1))))


(defun parse-line-string-serialized-session (line)
  (multiple-value-bind (element-name rem-char-list)
      (extract-element-name (coerce line 'list))
    (cons element-name 
	  (do ((elements nil))
	      ((not rem-char-list) elements)
	    (multiple-value-bind (result buff-char-list)
		(extract-element-content rem-char-list)
	      (setf rem-char-list buff-char-list)
	      (push result elements))))))
    
    
(defun parse-string-serialized-session (stream)
  (do ((line (read-line stream nil nil)
	     (read-line stream nil nil))
       (session-elements nil))
      ((not line) session-elements)
    (push (parse-line-string-serialized-session line) session-elements)))
