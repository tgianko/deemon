(in-package :de.uni-saarland.syssec.analyzer)


(opts:define-opts
  (:name :source-database-mosgi
	 :description "the database path of mosgis db from which to retrieve the information to analyze"
	 :short #\m
	 :long "source-database-mosgi"
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
	 :arg-parser #'parse-integer)
  (:name :keep-all-queries-p
         :description "all SQL queries are kept and non state-changing ones are not removed (y/N)"
         :short #\k 
         :long "keep-all-queries"
         :arg-parser #'(lambda (string)
                         (if (or (string= string "y")
                                 (string= string "Y"))
                             T
                             NIL))))
                             

(defun make-diff (id-list db-source-connection db-sink-connection keep-all-queries-p)
  (do ((rem-ids id-list (cdr rem-ids)))
      ((not rem-ids) nil)
    (FORMAT T "php session analysis for request ~a/~a~%" (car rem-ids) (car (last rem-ids)))
    (database:commit-raw-sessions 
     (car rem-ids)
     (database:get-all-session-entries (car rem-ids) db-source-connection)
     db-sink-connection) 
    (FORMAT T "xdebug analysis for request ~a/~a~%" (car rem-ids) (car (last rem-ids)))
    (let ((xdebug (xdebug:make-xdebug-trace-from-file (database:get-xdebug-entry-as-file-path (car rem-ids) db-source-connection))))
      (database:commit-sql-queries db-sink-connection 
                                   (car rem-ids) 
                                   (xdebug:get-sql-queries xdebug keep-all-queries-p)))))


(defmacro aif (condition then &optional else)
  `(let ((it ,condition))
     (if it
	 ,then
	 ,else)))


(defun generate-id-range (start end)
  (if (= start end)
      (cons start nil)
      (cons start
            (generate-id-range (+ start 1)
                               end))))


(defun main ()
  (handler-case 
      (multiple-value-bind (options free-args)
	  (opts:get-opts)
	(declare (ignore free-args))
	(let ((source-database-path-mosgi (aif (getf options :source-database-mosgi) 
                                               it 
                                               (error "source database path mosgi has to be provided")))
	      (sink-database-path (aif (getf options :sink-database) 
                                       it 
                                       (error "sink database path has to be provided")))
	      (sink-database-schema (aif (getf options :sink-database-schema) 
                                         it 
                                         (error "sink database schema has to be provided")))
	      (start-id (aif (getf options :start-id) 
                             it 
                             (error "the start-id has to be provided")))
              (end-id (aif (getf options :end-id) 
                           it 
                           (error "the end-id has to be provided")))
              (keep-all-queries-p (aif (getf options :keep-all-queries-p) 
                                       it 
                                       NIL)))	  
	  (clsql:with-database (source-db-mosgi (list source-database-path-mosgi) :database-type :sqlite3)
	    (clsql:with-database (sink-db (list sink-database-path) :database-type :sqlite3)
	      (database:create-database sink-db sink-database-schema)
              (make-diff (generate-id-range start-id end-id) source-db-mosgi sink-db keep-all-queries-p)))))
  (unix-opts:unknown-option (err)
    (declare (ignore err))
    (opts:describe
     :prefix "This program extracts the relevant data previously acquired from mosgi during a csrf test"
     :suffix "so that's how it works…"
     :usage-of "run-analyzer.sh"))))
