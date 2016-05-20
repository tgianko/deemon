#|
Author:Simon Koch <s9sikoch@stud.uni-saarland.de>
This file provides code to generate and diff file-history-states for
clustering of page requests
|#
(in-package :de.uni-saarland.syssec.mosgi.diff)
	   

(defstruct file-diff-entry diffs)


(defclass file-history-state ()
  ((file-index 
    :initarg :file-index
    :reader file-index)))


(defmethod print-object ((object file-history-state) stream)
  (FORMAT stream "[~%~{~{~a:~a~}~%~}]" (file-index object)))
   

(defun make-file-history-state (relevant-file-paths login-credentials)
  (make-instance 'file-history-state
		 :file-index (destructuring-bind (user host pwd)
				 login-credentials
			       (sort 
				(mapcar #'(lambda (file-path)
					    (cl-fad:with-open-temporary-file (temp-file-stream)
					      (ssh:scp file-path (pathname temp-file-stream) user host pwd)
					      (finish-output temp-file-stream)
					      (list file-path (file-length temp-file-stream))))
					relevant-file-paths)
				#'string<=
				:key #'car))))
  

(defmethod diff-history-state ((old file-history-state) (new file-history-state))
  (labels ((index-diff (old-index new-index) ;assumes string<= ordered indexes - true by definition of file-history-state
	     (cond 
	       ((not old-index)
		(mapcar #'(lambda (index-element) (cons (car index-element) (cdr index-element))) new-index))
	       ((not new-index)
		(mapcar #'(lambda (index-element) (cons (car index-element) (* -1 (cdr index-element)))) old-index))
	       ((and (string= (caar old-index) (caar new-index))
		     (not (= (cadar old-index) (cadar new-index))))
		(cons (cons (caar old-index) (- (cadar old-index) (cadar new-index)))
		      (index-diff (cdr old-index) (cdr new-index))))
	       ((string< (caar old-index) (caar new-index))
		(cons (cons (caar old-index) (- 0 (cadar old-index)))
		      (index-diff (cdr old-index) new-index))) ;file deleted
	       ((string> (caar old-index) (caar new-index))
		(cons (cons (caar new-index) (cadar new-index))
		      (index-diff old-index (cdr new-index))))
	       (T
		(index-diff (cdr old-index) (cdr new-index))))))
    (make-file-diff-entry :diffs (index-diff (file-index old) (file-index new)))))
