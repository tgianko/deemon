(defparameter *php-session-diff-state* nil)

(defparameter *file-diff-state* nil) 


(defun make-diff (user host pwd sqlite-db-path request-db-id)
  (let ((xdebug-trace-folder (FORMAT nil "/tmp/xdebug-trace-~a/" request-db-id))
	(php-session-folder (FORMAT nil "/tmp/php-sessions-~a/" request-db-id)))
    (handler-case
	(progn
	  (print-threaded :differ (FORMAT nil "php session analysis for request ~a" request-db-id))
	  (diff:add-next-state-* *php-session-diff-state* 
				 (diff:make-php-session-history-state php-session-folder user host pwd #'(lambda(string)
													   (print-threaded :differ string))))
	  (ssh:delete-folder php-session-folder user host pwd)
	  (cl-fad:with-open-temporary-file (xdebug-tmp-stream :direction :io :element-type 'character)
	    (print-threaded :differ (FORMAT nil "scp xdebug file for request ~a" request-db-id))
	    (ssh:scp (xdebug:get-xdebug-trace-file (ssh:folder-content-guest xdebug-trace-folder
									     user host pwd))
		     (pathname xdebug-tmp-stream) user host pwd)
	    (finish-output xdebug-tmp-stream)
	    (print-threaded :differ (FORMAT nil "scp'd xdebug file"))
	    (ssh:convert-to-utf8-encoding (namestring (pathname xdebug-tmp-stream))) ;this is just because encoding is stupid
	    (print-threaded :differ (FORMAT nil "parsing xdebug file for request ~a" request-db-id))
	    (let ((xdebug (xdebug:make-xdebug-trace xdebug-tmp-stream)))
	      (diff:add-next-state-* *file-diff-state* 
				     (diff:make-file-history-state 
				      (xdebug:get-changed-files-paths 
				       xdebug)
				      user host pwd))
	      (print-threaded :differ (FORMAT nil "entering xdebug results for request ~a" request-db-id))
	      (clsql:with-database (database (list sqlite-db-path) :database-type :sqlite3)
		(db-interface:commit-sql-queries database request-db-id (xdebug:get-sql-queries xdebug))
		(db-interface:commit-latest-diff database request-db-id *php-session-diff-state*)
		(db-interface:commit-full-sessions database request-db-id (diff:php-sessions (diff:current-state *php-session-diff-state*)))
		(db-interface:commit-latest-diff database request-db-id *file-diff-state*)))
	    (ssh:delete-folder xdebug-trace-folder user host pwd)
	    (print-threaded :differ (FORMAT nil "finished session analysis for request ~a" request-db-id))))
      (error (e)
	(print-threaded :differ (FORMAT nil "ERROR:~a" e))))))


(defun create-differ-thread (user host pwd sqlite-db-path)
  (sb-thread:make-thread #'(lambda()
			     (unwind-protect 
				  (let ((*file-diff-state* (make-instance 'diff:state-trace))
					(*php-session-diff-state* (make-instance 'diff:state-trace)))			       
				    (tagbody
				     check
				       (sb-thread:with-mutex (*task-mutex*)
					 (when *stop-p*
					   (go end))
					 (if (not (sb-concurrency:queue-empty-p *request-queue*))
					     (go work)
					     (sb-thread:condition-wait *task-waitqueue* *task-mutex*))
					 (go check))
				     work
				       (make-diff user host pwd sqlite-db-path (sb-concurrency:dequeue *request-queue*))
				       (print-threaded :differ (FORMAT nil "~a requests for processing remaining" (sb-concurrency:queue-count *request-queue*)))
				       (go check)
				     end))
			       (print-threaded :differ "I am done")))				
			 :name "differ"))
