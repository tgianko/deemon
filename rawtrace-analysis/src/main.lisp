(in-package :de.uni-saarland.syssec.analyzer)


(opts:define-opts
  (:name :source-database-mosgi
	 :description "the database path of mosgis db from which to retrieve the information to analyze"
	 :short #\m
	 :long "source-database-mosgi"
	 :arg-parser #'identity)
  (:name :source-database-vilanoo
	 :description "the database path of vilanoos db from which to retrieve the information to analyze"
	 :short #\v
	 :long "source-database-vilanoo"
	 :arg-parser #'identity)
  (:name :sink-database
	 :description "the database path into which to write the analyzed information"
	 :short #\d
	 :long "sink-database"
	 :arg-parser #'identity)
  (:name :sink-database-schema
	 :description "the schema for the sink database schema"
	 :short #\S 
	 :long "sink-schema"
	 :arg-parser #'identity)
  (:name :start-id 
	 :description "the id from which to start the analysis of the http requests"
	 :short #\f 
	 :long "start-id"
	 :arg-parser #'parse-integer)
  (:name :end-id 
	 :description "the last id to process while analyzing the http requests"
	 :short #\e 
	 :long "end-id"
	 :arg-parser #'parse-integer))


(defparameter *php-session-diff-state* nil)


(defparameter *file-diff-state* nil) 


(defun make-diff (id-list db-source-connection db-sink-connection)
  (database:copy-http-request-entries id-list db-source-connection db-sink-connection)
  (let ((*file-diff-state* (make-instance 'analysis:state-trace))
	(*php-session-diff-state* (make-instance 'analysis:state-trace)))
    (do ((rem-ids id-list (cdr rem-ids)))
	((not rem-ids) nil)
      (FORMAT T "php session analysis for request ~a/~a~%" (car rem-ids) (car (last rem-ids)))
      (analysis:add-next-state-* *php-session-diff-state*
				 (analysis:make-php-session-history-state (database:get-all-session-entries (car rem-ids) db-source-connection)))
      (FORMAT T "xdebug analysis for request ~a/~a~%" (car rem-ids) (car (last rem-ids)))
      (let ((xdebug (xdebug:make-xdebug-trace-from-file (database:get-xdebug-entry-as-file-path (car rem-ids) db-source-connection))))
	(analysis:add-next-state-* *file-diff-state* 
				   (analysis:make-file-history-state 
				    (xdebug:get-changed-files-paths 
				     xdebug)))
	(database:commit-sql-queries db-sink-connection (car rem-ids) (xdebug:get-sql-queries xdebug))
	(database:commit-latest-diff db-sink-connection (car rem-ids) *php-session-diff-state*)
	(database:commit-full-sessions db-sink-connection (car rem-ids) (analysis:php-sessions (analysis:current-state *php-session-diff-state*)))
	(database:commit-latest-diff db-sink-connection (car rem-ids) *file-diff-state*)))))


(defmacro aif (condition then &optional else)
  `(let ((it ,condition))
     (if it
	 ,then
	 ,else)))


(defun main ()
  (handler-case 
      (multiple-value-bind (options free-args)
	  (opts:get-opts)
	(declare (ignore free-args))
	(let ((source-database-path-vilanoo (aif (getf options :source-database-vilanoo) it (error "source database path vilanoo has to be provided")))
	      (source-database-path-mosgi (aif (getf options :source-database-mosgi) it (error "source database path mosgi has to be provided")))
	      (sink-database-path (aif (getf options :sink-database) it (error "sink database path has to be provided")))
	      (sink-database-schema (aif (getf options :sink-database-schema) it (error "sink database schema has to be provided")))
	      (start-id (aif (getf options :start-id) it 0)))
	  (FORMAT T "~a~%~a~%~a~%~a~%" source-database-path-mosgi source-database-path-vilanoo sink-database-path sink-database-schema)
	  (clsql:with-database (source-db-mosgi (list source-database-path-mosgi) :database-type :sqlite3)
	    (clsql:with-database (sink-db (list sink-database-path) :database-type :sqlite3)
	      (database:create-database sink-db sink-database-schema)
	      (clsql:with-database (source-db-vilanoo (list source-database-path-vilanoo) :database-type :sqlite3) ;;this is needed as vilanoo and mosgi use split db due to 
		(database:merge-databases source-db-vilanoo source-db-mosgi)) ;;stupid datarace problems of sqlite3
	      (let ((end-id (aif (getf options :end-id) it (+ (database:get-highest-http-request-id-entry source-db-mosgi) 1))))	    
		(make-diff (database:get-all-http-request-ids start-id end-id source-db-mosgi) source-db-mosgi sink-db))))))
  (unix-opts:unknown-option (err)
    (declare (ignore err))
    (opts:describe
     :prefix "This program is the badass doing all the work to differentiate state changes after actions on webapplications - kneel before thy master"
     :suffix "so that's how it works…"
     :usage-of "run-analyzer.sh"))))


  
