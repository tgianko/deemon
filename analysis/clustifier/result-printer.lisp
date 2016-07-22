(in-package :de.uni-saarland.syssec.clustifier.result-printer)


(defun clustify (http-request-list key)
  (let ((table (make-hash-table :test 'equalp)))
    (dolist (http-request http-request-list)
      (if (gethash (funcall key http-request) table)
	  (setf (gethash (funcall key http-request) table)
		(append (gethash (funcall key http-request) table) (list (database:http-request-id http-request))))
	  (setf (gethash (funcall key http-request) table)
		(list (database:http-request-id http-request)))))
    (let ((results nil))
      (maphash #'(lambda (key value)
		   (push (cons key value) results)) table)
      results)))
		   

;;Graham's aif
(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))


(defun deep-entry (http-request table)
  (let ((file-table (gethash (database:sql-hash http-request) table)))
    (if file-table
	(let ((session-table (gethash (database:file-hash http-request) file-table)))
	  (if session-table
	      (let ((entry (gethash (database:session-hash http-request) session-table)))
		(if entry 
		    (setf (gethash (database:session-hash http-request) session-table) 
			  (append (gethash (database:session-hash http-request) session-table) (list (database:http-request-id http-request))))
		    (setf (gethash (database:session-hash http-request) session-table) 
			  (list (database:http-request-id http-request)))))
	      (let ((new-session-table (make-hash-table :test 'equalp)))
		(setf (gethash (database:file-hash http-request) file-table) new-session-table)
		(deep-entry http-request table))))
	(let ((new-file-table (make-hash-table :test 'equalp)))
	  (setf (gethash (database:sql-hash http-request) table) new-file-table)
	  (deep-entry http-request table)))))


(defun deep-clustify (http-request-list)
  (let ((table (make-hash-table :test 'equalp)))
    (dolist (http-request http-request-list)
      (deep-entry http-request table))
    (let ((results nil))
      (maphash #'(lambda (sql-key file-table)
		   (maphash #'(lambda(file-key session-table)
				(maphash #'(lambda(session-key values)
					     (push (cons (list sql-key file-key session-key) values)
						   results))
					 session-table))
			    file-table))
	       table)
      results)))
    

(defun generate-http-request-file-dumps (http-requests-list folder-path)
  (mapcar #'(lambda(http-request)
	      (with-open-file (stream (FORMAT nil "~a/http-request-~a" folder-path (database:http-request-id http-request)) :if-does-not-exist :create :direction :output)
		(FORMAT stream "~a" http-request)))
	  http-requests-list))


(defun generate-sql-cluster-dump (http-request-list folder-path)
  (with-open-file (stream (FORMAT nil "~a/sql-cluster-dump" folder-path) :if-does-not-exist :create :direction :output)
    (FORMAT stream "~{~a~%~%~%~}" (clustify http-request-list #'database:sql-hash))))


(defun generate-file-cluster-dump (http-request-list folder-path)
  (with-open-file (stream (FORMAT nil "~a/file-cluster-dump" folder-path) :if-does-not-exist :create :direction :output)
    (FORMAT stream "~{~a~%~%~%~}" (clustify http-request-list #'database:file-hash))))


(defun generate-session-cluster-dump (http-request-list folder-path)
  (with-open-file (stream (FORMAT nil "~a/session-cluster-dump" folder-path) :if-does-not-exist :create :direction :output)
    (FORMAT stream "~{~a~%~%~%~}" (clustify http-request-list #'database:session-hash))))


(defun generate-full-cluster-dump (http-request-list folder-path)
  (with-open-file (stream (FORMAT nil "~a/full-cluster-dump" folder-path) :if-does-not-exist :create :direction :output)
    (mapcar #'(lambda(entry)
		(FORMAT stream "<~{~a~^,~}> ~{~a~^,~}~%~%~%" (car entry) (cdr entry)))
	    (deep-clustify http-request-list))))


(defun dump-results-into-folder (http-request-list folder-path)
  (ensure-directories-exist folder-path)
  (generate-http-request-file-dumps http-request-list folder-path)
  (generate-sql-cluster-dump http-request-list folder-path)
  (generate-file-cluster-dump http-request-list folder-path)
  (generate-session-cluster-dump http-request-list folder-path)
  (generate-full-cluster-dump http-request-list folder-path))
	    
